#' #' Data from the queries available in SMART Connect
#' 
#' @details
#' This function is relevant to load the data from SMART Connect in the R 
#' session using the "data_from_connect" function. Not all SMART queries have 
#' the same properties. The query_info function provides the properties of each 
#' query available for each conservation area. The 'folder' column provides the 
#' the to the folder where the query is stored in the conservation area. The 
#' 'query_name' column provides the query name. The 'typeKey' provides the type 
#' of query. The 'uuid' has an internal SMART identifier for each query relevant 
#' for the API call to load the output of the query. The 'spatial_query' is a
#' column with boolean values where TRUE means 'the query has spatial
#' information' and FALSE otherwise. The following 'date_filter_type' columns 
#' are also boolean columns showing the referential temporal filters options 
#' available. For example, 'date_filter_type:waypointdate' equals TRUE indicates 
#' that waypointdate can be selected as reference for the start and end 
#' date filters of the data to be included in the query.  The output of this
#' function is needed to provide the correct arguments to the 
#' 'data_from_connect' function.
#' 
#' @param server_url A string with the URL of the SMART Connect server 
#' (e.g., "https://wcshealth.smartconservationtools.org/server" )
#' @param user A string with corresponding SMART Connect username 
#' (e.g., "connect_username")
#' @param password A string with the corresponding SMART Connect user's password
#'  (e.g., "my_connect_password")
#'  
#' @return A list of tibbles. The length of the list corresponds to the number 
#' SMART conservation areas the user has access to. Each tibble has the following
#' columns (see details for an explanation of each one of them): 
#' folder, query_name, typeKey, uuid,  spatial_query, 
#' "date_filter_type:assetdeploymentdate", "date_filter_type:missionenddate",
#' "date_filter_type:missionstartdate", "date_filter_type:missiontrackdate",
#' "date_filter_type:patrolend", "date_filter_type:patrolstart", 
#' "date_filter_type:RecordDate", "date_filter_type:waypointdate",
#' "date_filter_type:waypointlastmodified".
#' 
#' @export
#'
#' @examples
#' user<-"your_smart_connect_user_name"
#' password<-"your_smart_connect_password"
#' server_url<-"https://wcshealth.smartconservationtools.org/server"
#' query_data<-query_info(user=user, password=password, server_url=server_url)


# ------------------------------------------------------------------------#
# function to check the conservation areas in connect you have access to  #
# and the corresponding queries within that conservation area             #
# ------------------------------------------------------------------------#

query_info<-function(

     server_url, # your connect server URL as character (e.g., "https://wcshealth.smartconservationtools.org/server") 
     user,      # your connect username as character. 
     password){ # your connect password
  
  #open connect. The login page
  session_connect <- rvest::session(server_url)
  
  #provide the username and password
  form.unfilled <- session_connect %>% 
    rvest::html_node("form") %>% 
    rvest::html_form()
  
  form.filled = form.unfilled %>%
    rvest::html_form_set("j_username" = user,
                         "j_password" = password)
  
  #login
  logged.in.connect <- rvest::session_submit(session_connect, form.filled) 
  
  # get the set of the queries available with their unique identifier
  api.queries <- rvest::session_jump_to(logged.in.connect, 
                                        paste0(server_url, "/api/query/tree"))
  
  if(length( api.queries$response %>% rvest::read_html())==0){
    stop("there are no conservation areas available in connect")}
   
  #query data
  api.queries.2<-api.queries$response %>% 
    rvest::read_html() %>% 
    rvest::html_text() %>% 
    jsonlite::fromJSON(simplifyVector = F)
  
  #names of the conservation areas in connect and with queries available
  names.conservation.areas<-purrr::map_vec(api.queries.2, function(x) x$name)
  
  #queries by conservation area by folder,including folders wo any queries
  api.queries.3<-
    purrr::map(seq_along(api.queries.2), function(x){
      
      #if there are not queries in the ca folder
      if(is.na(dplyr::tibble(query=api.queries.2[x]) %>% 
               tidyr::unnest_wider(query) %>% 
               dplyr::rename(folder=name) %>% 
               dplyr::select(-caUuid) %>% 
               dplyr::pull(items))){
        
        
            list( # list a tibble with query name TypeKey and uuid as NA
              dplyr::as_tibble(data.frame(folder=dplyr::tibble(query=api.queries.2[x]) %>% #
                                     tidyr::unnest_wider(query) %>% 
                                     dplyr::rename(folder=name) %>% 
                                     dplyr::pull(folder),# the CA name
                                   caUuid=dplyr::tibble(query=api.queries.2[x]) %>% 
                                     tidyr::unnest_wider(query) %>% 
                                     dplyr::rename(folder=name) %>% 
                                     dplyr::pull(caUuid), # the CA code
                                   query_name=NA, 
                                   typeKey=NA, 
                                   uuid=NA))
              
              %>%  # but add the subfolder data as the last column
                    # which may contain other folders that might
                    # contain queries or not
                dplyr::bind_cols(dplyr::tibble(query=api.queries.2[x]) %>% 
                            tidyr::unnest_wider(query) %>% 
                            dplyr::rename(folder=name) %>% 
                            dplyr::select(subFolders)))}else{
                          
      #otherwise, if the CA folder has queries, list the data of the CA folder
      # these data includes queries in the folders and other subfolders with 
      # or wo queries                        
      list(dplyr::tibble(query=api.queries.2[x]) %>% 
             tidyr::unnest_wider(query) %>% 
             dplyr::rename(folder=name) %>% 
             dplyr::select(-caUuid) %>% 
             tidyr::unnest(items) %>% 
             tidyr::unnest_wider(items) %>% 
             dplyr::select(-c( type, id, isShared, folderUuid, iconName, isCcaa, conservationArea)) %>% 
             dplyr::rename(query_name=name) %>% 
             dplyr::select(folder, caUuid, query_name, typeKey, uuid, subFolders))}})
  
  
  #get the data from the queries in the query folders or in folders within query 
  #folders
  for(i in seq_along(api.queries.3)){
    
    counter <- 1
    
    # the while understands that there are subfolders nested with data
    while(!all(is.na(api.queries.3[[i]][[counter]]$subFolders) | 
               is.null(api.queries.3[[i]][[counter]]$subFolders))){ 
      #> if not all subfolders are empty or
      #> if at least one CA has a subfolder
      
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
        dplyr::filter_all(dplyr::any_vars(!is.na(.)))

      #create the full path to each query
      api.queries.3[[i]][[counter+1]]$folder<-
        paste0(rep(api.queries.3[[i]][[counter]]$folder,  
                   map_vec(api.queries.3[[i]][[counter]]$subFolders, length)),
               "/",
               api.queries.3[[i]][[counter+1]]$folder)
      
      
      counter <- counter + 1
    }
    
    
  }
  
  #join the data per CA and remove rows wo queries
  api.queries.4<-purrr::map(api.queries.3, \(x) x %>% 
                              purrr::reduce(dplyr::full_join) %>%  
                              dplyr::filter(!is.na(query_name)) %>% 
                              dplyr::select(-caUuid, -subFolders))
  
  
  #checking if any query is available for any CA    
  if(all(purrr::map_vec(sapply(api.queries.4, "[[", "query_name"), length)==0)){
    stop("there are no queries available in connect")}

  #add information about spatial queries
  api.queries.5<-purrr::map(api.queries.4,  ~ .x %>%
  
    # learn if the query hs spatial information
    dplyr::mutate(spatial_query=typeKey%in%
          c("entityobservation", "entitywaypoint","intelligencerecord",  
            "surveymission",
            "surveymissiontrack", "observationobservation", "observationwaypoint", 
            "patrolobservation", "patrolquery", "patrolwaypoint", 
            "surveyobservation", "surveywaypoint",
            "assetobservation", "assetwaypoint")))
  
  
  # add the date filter options for each query
  
  # date_filter_types_available_per_query<-source("data-raw/build_query_type_data.R")
  # load("data/date_filter_types_available_per_query_type.rda")
  
  api.queries.6 <- 
    purrr::map(api.queries.5, \(x)
        dplyr::left_join(x, 
                         date_filter_types_available_per_query_type, 
                         by = "typeKey"))
  
  
   # assign the names of the conservation areas
  names(api.queries.6)<-names.conservation.areas
  
  
  # see all query data for all the conservation areas that have at least one query available
  return(api.queries.6)}
