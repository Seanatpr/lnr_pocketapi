library(yaml)
library(tidyverse)
library(purrr)
library(lubridate)
library(jsonlite)
library(httr)


# Load configation --------------------------------------------------------
DIR_CONFIG <- './cfg'
FILE_CONFIG <- 'prj_cfg.txt'
.CONFIGS <- yaml.load_file(file.path(DIR_CONFIG, FILE_CONFIG))


# Set up dir ------------------------------------------------------------------
DIR_PRJBASE <- .CONFIGS$paths$project_dir
setwd(DIR_PRJBASE)

DIR_INPUT <-  file.path(DIR_PRJBASE, .CONFIGS$paths$input_dir)
DIR_MIDPUT <- file.path(DIR_PRJBASE, .CONFIGS$paths$midput_dir)
DIR_OUTPUT <- file.path(DIR_PRJBASE, .CONFIGS$paths$output_dir)


# Set up secruity ---------------------------------------------------------
.TOKEN_API_POCKET <- yaml.load_file(file.path(DIR_CONFIG, .CONFIGS$files$api_token))
TOKEN_POCKET_CONSUMER_KEY <- .TOKEN_API_POCKET$api_auth_consumer_key$windows_desktop


# API URL base ------------------------------------------------------------
.API_URL_BASE <- paste(c('https://getpocket.com', 'v3'), collapse = '/')
API_URL_AUTH <- paste(c(.API_URL_BASE, 'oauth'), collapse = '/')
API_URL_GET <- paste(c(.API_URL_BASE, 'get'), collapse = '/')
API_URL_SEND <- paste(c(.API_URL_BASE, 'send'), collapse = '/')

# API_HEADER <- list('content-type' = 'application/json; charset=UTF-8',
#                    'x-accept' = 'application/json')

# apiUrl <- function(...){
#     return(str_c('urlbase', ..., sep = "/"))
# }
# 
# print(apiUrl('a', 'b', 'b'))


# Get all archived post id ------------------------------------------------
# https://getpocket.com/developer/docs/authentication
# Step 2: Obtain a request token
api_call_url <- paste(c(API_URL_AUTH, 'request'), collapse = '/')
api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                      redirect_uri = 'sp_pocket_app:authorizationFinished')

api_call_response <- POST(url = api_call_url, 
                          body = api_call_body)

request_token <- ''
if(api_call_response$status_code == 200){
    api_result <- rawToChar(api_call_response$content)    
    if(str_sub(api_result, 1, 5) == 'code='){
        api_result <- str_sub(api_result, 6, -1)
        if(nchar(api_result) == 30){
            request_token <- api_result
        }
    }
}

stopifnot(nchar(request_token)>0)

# Step 3: Redirect user to Pocket to continue authorization
api_call_auth_url <- paste0('https://getpocket.com/auth/authorize?request_token=',
                            request_token, 
                            '&redirect_uri=sp_pocket_app:authorizationFinished')
browseURL(api_call_auth_url, browser = getOption("browser"), encodeIfNeeded = FALSE)
readline(prompt="Press [enter] to continue")

# Step 5: Convert a request token into a Pocket access token
api_call_url <- paste(c(API_URL_AUTH, 'authorize'), collapse = '/')
api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                      code = request_token)

api_call_response <- POST(url = api_call_url, 
                          body = api_call_body)


TOKEN_POCKET_ACCESS_TOKEN <- ''
TOKEN_POCKET_USER_NAME <- ''
if(api_call_response$status_code == 200){
    api_result <- rawToChar(api_call_response$content)    
    #api_result <- "access_token=ecc73859-2efe-7f92-e955-cbfd92&username=pplluuss"
    api_result <- str_split(api_result, '&', simplify = TRUE)
    TOKEN_POCKET_ACCESS_TOKEN <- str_sub(api_result[1], 14, -1)
    TOKEN_POCKET_USER_NAME <- str_sub(api_result[2], 10, -1)    
}

stopifnot(nchar(TOKEN_POCKET_ACCESS_TOKEN)>0)
stopifnot(nchar(TOKEN_POCKET_USER_NAME)>0)




# Get archived posts ------------------------------------------------------
# https://getpocket.com/developer/docs/v3/retrieve
api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                      access_token = TOKEN_POCKET_ACCESS_TOKEN,
                      state = 'archive',
                      detailType = 'simple')

api_call_response <- POST(url = API_URL_GET, 
                          body = api_call_body)
api_result <- rawToChar(api_call_response$content)  
api_result <- fromJSON(api_result)
api_result <- api_result$list


extractItemInfo <- function(aItem){
    if(aItem$resolved_id == '0'){
        return(NULL)
    }
    
    return(data.frame(item_id =  coalesce(aItem$resolved_id, ''),
                      created_at = coalesce(aItem$time_added, '0'),
                      item_url = coalesce(aItem$resolved_url, ''),
                      item_title = coalesce(aItem$resolved_title, ''),
                      stringsAsFactors = FALSE))
}

n <- length(api_result)
result <- vector('list', n)
for(i in 1:n) {
    result[[i]] <- extractItemInfo(api_result[[i]])
}
result <- do.call('rbind', result)
result %>% 
    filter(!is.na(item_id) &&
           nchar(item_id)>0) %>% 
    mutate(created_at = as_datetime(as.integer(created_at))) ->
    result

# delete ------------------------------------------------------------------
# https://getpocket.com/developer/docs/v3/modify#action_delete
result %>% 
    mutate(action = 'delete') %>% 
    select(action, item_id) -> x1


api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                      access_token = TOKEN_POCKET_ACCESS_TOKEN,
                      actions = toJSON(transpose(x1), auto_unbox = TRUE))

api_call_response <- POST(url = API_URL_SEND, 
                          body = api_call_body)

api_result <- content(api_call_response)
