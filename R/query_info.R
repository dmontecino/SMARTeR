#' Properties of the queries available in SMART Connect
#' 
#' A function to learn about the queries available in SMART Connect and their 
#' properties. This information is relevant to provide the correct parameters 
#' for the function 'data_from_connect' and load the corresponding query output
#' in the R session. 
#' 
#' @details
#' Not all SMART queries have the same properties. The 'query_info function' 
#' provides the properties of the queries in each conservation area 
#' following the permissions of SMART Connect users. 
#' 'folder' column provides the
#' query folder of the conservation area where the query is stored. 
#' query_name' provides the query name. 
#' 'typeKey' provides the type of query. 
#' 'uuid' is an internal SMART identifier for the query 
#' 'spatial_query' is a column with T/F values where TRUE means 
#' the query has spatial information and otherwise if FALSE. 
#' The 'date_filter_type:' features are also T/F showing the
#' available temporal filters options of the query. For example, a
#' 'date_filter_type:waypointdate' equals TRUE means that 'waypointdate' can 
#' be chosen as the reference for the start and end dates of the data to 
#' be included in the query output when using function data_from_connect'. Avoid 
#' empty query folders.
#' 
#' @param server_url A string with the URL of the SMART Connect server 
#' (e.g., "https://wcshealth.smartconservationtools.org/server" )
#' @param user A string with corresponding SMART Connect username 
#' (e.g., "connect_username")
#' @param password A string with the corresponding SMART Connect user's password
#'  (e.g., "my_connect_password")
#'  
#' @return A list with nested tibbles. The length of the list corresponds to the number 
#' SMART conservation areas the user has access to. The number nested tibbles 
#' per each list object corresponds to the number of queries available per 
#' conservation area. Each nested tibble has the information of a query 
#' shown in two columns: "feature" and "value".
#' The features are: folder, query_name, typeKey, uuid,  spatial_query, 
#' date_filter_type:assetdeploymentdate, date_filter_type:missionenddate,
#' date_filter_type:missionstartdate, date_filter_type:missiontrackdate,
#' date_filter_type:patrolend, date_filter_type:patrolstart, 
#' date_filter_type:RecordDate, date_filter_type:waypointdate,
#' date_filter_type:waypointlastmodified. See the details section.
#' The objects are named so it is possible to find the information of a specific
#' query with list$conservation_area_name$query_name
#' 
#' @export
#'
#' @examples
#' user<-Sys.getenv("EXAMPLE_CONNECT_USERNAME")
#' password<-Sys.getenv("EXAMPLE_CONNECT_PASSWORD")
#' server_url = Sys.getenv("EXAMPLE_CONNECT_URL")
#' query_data<-query_info(user=user, 
#'                        password=password, 
#'                        server_url=server_url)
#' 
#' 
#' @seealso [data_from_connect]


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
    # for(i in 3){
    
    counter <- 1
  
  # the while understands that there are subfolders nested with data
  # while(!all(is.na(api.queries.3[[i]][[counter]]$subFolders) | 
  #            is.null(api.queries.3[[i]][[counter]]$subFolders))){ 
  
  if("subFolders"%in%colnames(api.queries.3[[i]][[counter]]) &
     !all(is.na(api.queries.3[[i]][[counter]]$subFolders))){ 
    
    #> if not all subfolders are empty or
    #> if at least one CA has a subfolder
    
    api.queries.3[[i]][[counter+1]]<-
      #queries per folder
      api.queries.3[[i]][[counter]] %>% 
      dplyr::select(folder, subFolders) %>% 
      tidyr::unnest(subFolders) %>% 
      tidyr::unnest_wider(subFolders) %>% 
      
      dplyr::distinct() %>% 
      dplyr::mutate(folder=paste0(folder, "/", name)) %>% 
      # dplyr::rename(query_name=name) %>% 
      
      # dplyr::relocate(subFolders, .after = last_col()) %>% 
      # dplyr::rename(folder=name) %>% 
      dplyr::select(-caUuid, -subFolders, -name) %>% 
      tidyr::unnest(items, keep_empty = T) %>% 
      tidyr::unnest_wider(items) %>% 
      dplyr::select(-c( type, id, isShared, folderUuid, iconName, isCcaa, conservationArea)) %>% 
      dplyr::rename(query_name=name) %>% 
      # dplyr::distinct() %>% 
      dplyr::filter_all(dplyr::any_vars(!is.na(.)))
    
    
    #create the full path to each query
    # api.queries.3[[i]][[counter+1]]$folder<-
    #   paste0(rep(api.queries.3[[i]][[counter]]$folder,  
    #              purrr::map_vec(api.queries.3[[i]][[counter]]$subFolders, length)),
    #          "/",
    #          api.queries.3[[i]][[counter+1]]$folder)
    
    
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
  
  api.queries.7<-purrr::map(api.queries.6, \(x) purrr::map(1:nrow(x), \(y) x[y,]))
  
  for(i in seq_along(api.queries.7)){
    for(y in seq_along(api.queries.7[[i]])){
      
      api.queries.7[[i]][[y]]<-
        tibble::tibble(
          feature =names(api.queries.7[[i]][[y]]),
          value= as.character(api.queries.7[[i]][[y]][1,]))}
    names(api.queries.7[[i]])<-purrr::map_vec(api.queries.7[[i]], \(x) x %>% 
                                dplyr::filter(feature=="query_name") %>% 
                                dplyr::pull(value))}
  
  
  # see all query data for all the conservation areas that have at least one query available
  return(api.queries.7)}
