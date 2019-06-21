source('prj_comm.R', local = TRUE)




# Get all archived post id ------------------------------------------------
results <- POST(API_URL_METHOD, body = TOKEN_POCKET_BODY)
content(results)




# Get archived posts ------------------------------------------------------
# https://getpocket.com/developer/docs/v3/retrieve
getArchivedFromAPI <- function(){
    api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                          access_token = TOKEN_POCKET_ACCESS_TOKEN,
                          state = 'archive',
                          detailType = 'simple')
    
    api_call_response <- POST(url = API_URL_GET, 
                              body = api_call_body)
    api_result <- content(api_call_response)
    api_result <- fromJSON(api_result)
    return(api_result$list)
}


extractItemInfo <- function(aItem){
    if(aItem$resolved_id == '0'){
        return(NULL)
    }
    
    return(c(coalesce(aItem$resolved_id, ''),
             coalesce(aItem$time_added, '0'),
             coalesce(aItem$resolved_url, ''),
             coalesce(aItem$resolved_title, '')))
}

parseApiResultArchived <- function(aApiResult){
    n <- length(aApiResult)
    ret <- vector('list', n)
    for(i in 1:n) {
        ret[[i]] <- extractItemInfo(aApiResult[[i]])
    }
    ret <- as.data.frame(t(do.call('rbind', result)), stringsAsFactors = FALSE)
    
    ret %>% 
        filter(!is.na(item_id) && nchar(item_id)>0) %>% 
        mutate(created_at = as_datetime(as.integer(created_at))) %>% 
        return()
}

apiResultArchived <- getArchivedFromAPI()
df_archived <- parseApiResultArchived(apiResultArchived)


# delete ------------------------------------------------------------------
# https://getpocket.com/developer/docs/v3/modify#action_delete
df_archived %>% 
    mutate(action = 'delete') %>% 
    select(action, item_id) -> 
    toDelete

api_call_body <- list(consumer_key = TOKEN_POCKET_CONSUMER_KEY,
                      access_token = TOKEN_POCKET_ACCESS_TOKEN,
                      actions = toJSON(transpose(toDelete), auto_unbox = TRUE))

api_call_response <- POST(url = API_URL_SEND, 
                          body = api_call_body)

api_result <- content(api_call_response)
