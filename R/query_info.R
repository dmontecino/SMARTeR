
library(rvest)
library(plyr)
library(dplyr)
# library(tibble)
library(jsonlite)
library(purrr)


# ------------------------------------------------------------------------#
# function to check the conservation areas in connect you have access to  #
# and the corresponding queries within that conservation area             #
# ------------------------------------------------------------------------#

query_info<-function(

     server_url, # your connect server URL as character (e.g., "https://wcshealth.smartconservationtools.org/server") 
     user,      # your connect username as character. 
     password){ # your connect password
  
  #open connect. The login page
  session_connect <- session(server_url)
  
  #provide the username and password
  form.unfilled <- session_connect %>% html_node("form") %>% html_form()
  
  form.filled = form.unfilled %>%
    html_form_set("j_username" = user,
                  "j_password" = password)
  
  #login
  logged.in.connect <- session_submit(session_connect, form.filled) 
  
  # get the set of the queries available with their unique identifier
  api.queries <- session_jump_to(logged.in.connect, paste0(server_url, "/api/query/tree"))
  
  if(length( api.queries$response %>% read_html())==0){
    stop("there are no conservation areas available in connect")}
   
  #queries available 
  api.queries.2<-api.queries$response %>% read_html() %>% html_text() %>% fromJSON(simplifyVector = F)
  
  #names of the conservation areas in connect and with queries available
  names.conservation.areas<-sapply(api.queries.2, function(x) x$name)
  
  #queries by conservation area by folder,including folders wo any queries
  api.queries.3<-
    map(seq_along(api.queries.2), function(x){
      list(dplyr::tibble(query=api.queries.2[x]) %>% 
             tidyr::unnest_wider(query) %>% 
             dplyr::rename(folder=name) %>% 
             dplyr::select(-caUuid) %>% 
             tidyr::unnest(items) %>% 
             tidyr::unnest_wider(items) %>% 
             dplyr::select(-c( type, id, isShared, folderUuid, iconName, isCcaa, conservationArea)) %>% 
             dplyr::rename(query_name=name) %>% 
             dplyr::select(folder, caUuid, query_name, typeKey, uuid, subFolders))})
  
  
  
  for(i in seq_along(api.queries.3)){
    
    counter <- 1
    
    while(!all(is.na(api.queries.3[[i]][[counter]]$subFolders) | 
               is.null(api.queries.3[[i]][[counter]]$subFolders))){ 
      #> if not all subfolders are empty
      #> if at least one CA has a  subfolder

      api.queries.3[[i]][[counter+1]]<-
        #queries per folder
        api.queries.3[[i]][[counter]] %>% 
        dplyr::select(subFolders) %>% 
        tidyr::unnest(subFolders) %>% 
        tidyr::unnest_wider(subFolders) %>% 
        dplyr::relocate(subFolders, .after = last_col()) %>% 
        dplyr::rename(folder=name) %>% 
        dplyr::select(-caUuid) %>% 
        tidyr::unnest(items, keep_empty = T) %>% 
        tidyr::unnest_wider(items) %>% 
        dplyr::select(-c( type, id, isShared, folderUuid, iconName, isCcaa, conservationArea)) %>% 
        dplyr::rename(query_name=name) %>% 
        dplyr::distinct() %>% 
        dplyr::filter_all(any_vars(!is.na(.)))
      
      api.queries.3[[i]][[counter+1]]$folder<-
        paste0(rep(api.queries.3[[i]][[counter]]$folder,  
                   map_vec(api.queries.3[[i]][[counter]]$subFolders, length)),
               "/",
               api.queries.3[[i]][[counter+1]]$folder)
      
      
      counter <- counter + 1
    }
    
    
  }
  
  api.queries.4<-map(api.queries.3, \(x) x %>% 
                 reduce(full_join) %>%  
                 dplyr::select(-caUuid, -subFolders))
  
  
  #checking if any query is available for any CA    
  if(all(sapply(sapply(api.queries.4, "[[", "query_name"), length)==0)){
    stop("there are no queries available in connect")}

  #add information about spatial queries
  api.queries.5<-map(api.queries.4,  ~ .x %>%
  
    # learn if the query hs spatial information
    mutate(spatial_query=typeKey%in%
          c("entityobservation", "entitywaypoint","intelligencerecord",  "surveymission",
            "surveymissiontrack", "observationobservation", "observationwaypoint", 
            "patrolobservation", "patrolquery", "patrolwaypoint", 
            "surveyobservation", "surveywaypoint",
            "assetobservation", "assetwaypoint")))
  
  
  # add the date filter options for each query
  
  api.queries.6 <- 
    map(api.queries.5, \(x)
        left_join(x, date_filter_types_available_per_query, by = "typeKey"))
  
  
   # assign the names of the conservation areas
  names(api.queries.6)<-names.conservation.areas
  
  
  # see all query data for all the conservation areas that have at least one query available
  return(api.queries.6)}
