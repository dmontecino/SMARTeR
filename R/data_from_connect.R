#' Run a SMART query in SMART Connect and open the output in an R session. 
#' 
#' A function to load data from queries available in SMART Connect in specific
#' Conservation Areas. Depending on the type of query, it is possible to load
#' the data as a spatial object. To learn more about the properties of your
#' queries, use the function "query_info". 
#' 
#' @details
#' Not all SMART queries have spatial information included. Different query
#' types have different properties including day filter options. To learn about
#' the Conservation Areas available in SMART Connect, the queries available per 
#' Conservation Area, and their properties such as query type, spatial 
#' information availability (the type_output parameter of this function), and 
#' date filter options (the date_filter parameter of this function) use the 
#' "query_info" function first. Avoid empty query folders.
#'
#' @param server_url A string with the URL of the SMART Connect server 
#' (e.g., "https://wcshealth.smartconservationtools.org/server" )
#' @param user A string with corresponding SMART Connect username 
#' (e.g., "connect_username")
#' @param password A string with the corresponding SMART Connect user's password
#'  (e.g., "my_connect_password")
#' @param name_conservation_area A string he name of the Conservation Area 
#' holding the query
#' @param query_name A string with the name of the SMART query to run 
#' (e.g., "query_name")
#' @param directory a path to a directory with the proper permissions to write 
#' a temp file with the spatial data. For now needed when the "shp
#' output is selected
#' @param type_output Either "csv" or "shp" to load the query data as 
#' a tibble or sf object. Not all queries have spatial data. See details. 
#' @param date_filter A string with the field establishing the start and 
#' end dates of the data of the query's output. Default is "waypointdate". 
#' See details.
#' @param start_date A string with the start date filter with the format 
#' YYYY-MM-DD (e.g, "2000-01-01"). Default is NULL.
#' @param end_date A string with the end date filter with the format 
#' YYYY-MM-DD (e.g, "2000-01-01"). Default is NULL.
#' @param srid An integer with the EPSG code of the projection for a spatial 
#' output (when "shp" is selected). Default is 4326.
#' @param UTM_zone An integer to provide the Zone if a Universal Transverse 
#' Mercator is the srid input. From 1 to 60. 
#'
#' @return A tibble or sf object with the data returned by the query run 
#' in SMART Connect. Columns and rows are returned as defined in the source
#' query. If summary queries are requested, the output does not have adequate
#' column names and only the very fist header is considered a column. In any case
#' if you are loading the data in R it is because you want to summarize data here.
#' This function should be use for queries returning raw data (patrol queries,
#' patrol observation queries, etc.).
#' 
#' @export
#'
#' @examples
#' 
#' server_url <- Sys.getenv("EXAMPLE_CONNECT_URL")
#' user <- Sys.getenv("EXAMPLE_CONNECT_USERNAME")
#' password <-Sys.getenv("EXAMPLE_CONNECT_PASSWORD")
#' directory <- Sys.getenv("EXAMPLE_DIRECTORY")
#'                            
#'data<-data_from_connect(server_url = server_url,
#'                            user = user, 
#'                            password = password,
#'                            name_conservation_area = "Example Conservation Area [SMART]",
#'                            query_name = "Patrol_query",
#'                            type_output = "shp",
#'                            directory = directory,
#'                            date_filter="waypointlastmodified",
#'                            start_date="2020-01-01", #YYYY-MM-DD
#'                            end_date="2023-07-31", #YYYY-MM-DD
#'                          srid=4326,
#'                          UTM_zone=NULL)
#' 
#' @seealso
#' [query_info()] to learn about the Conservation 
#' Areas available in SMART Connect, the queries in each Conservation Area, the 
#' type of each query, spatial data in the queries, and the options available to
#' filter by date for each query.
#' ,
data_from_connect<-function(server_url, 
                            user, 
                            password, 
                            name_conservation_area,
                            query_name,
                            type_output,
                            directory,
                            date_filter="waypointdate",
                            start_date=NULL, #YYYY-MM-DD as character
                            end_date=NULL, #YYYY-MM-DD as character
                            srid=4326, 
                            UTM_zone=NULL){

  
  # if the date_filter is set to null or na     
  
  if(is.null(date_filter) | is.na(date_filter)){
    stop("'date_filter' must be provided")}
  
  #if the selected dates are in the future
  if(!is.null(start_date)){
    if(start_date>Sys.Date()){
      stop("'start_date' cannot go beyond today")}}
  
  if(!is.null(end_date)){
    if(end_date>Sys.Date()){
      stop("'end_date' cannot go beyond today")}}
  
  
  # --------------------------------------------------------------------------------------------------------- #
  # function to save the data as a temp, unzip it, and open it as a sf object when the type selected is "shp" #
  # --------------------------------------------------------------------------------------------------------- #

  dlshape=function(shploc, shpfile) {
    temp <- tempfile()
    writeBin(shploc, temp)
    utils::unzip(temp, exdir = directory) #Extract files from zip archive.
    fp <- sf::read_sf(file.path(directory, shpfile)) # Read the shapefile using sf::read_sf
    unzipped_files <- list.files(directory, pattern = "\\.fix|\\.shp|\\.dbf|\\.shx|\\.prj", full.names = TRUE)
    file.remove(unzipped_files)
    return(fp)}
  
  # -------------------------------------------------------------------------- #
  # go to Connect to find information about the requested query and run checks #
  # -------------------------------------------------------------------------- #
  
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
  
  #queries available 
  api.queries.2<-api.queries$response %>% 
    rvest::read_html() %>% 
    rvest::html_text() %>% 
    jsonlite::fromJSON(simplifyVector = F)
  
  #names of the conservation areas in connect and with queries available
  names.conservation.areas<-purrr::map_vec(api.queries.2, function(x) x$name)
  
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
                   purrr::map_vec(api.queries.3[[i]][[counter]]$subFolders, length)),
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
  
  
  
  names(api.queries.5)<-names.conservation.areas
  
  
  # --- FIND THE SPECIFIED QUERY --- #
  
  # select the specific query uuid
  query.api.number<-api.queries.5[[name_conservation_area]] %>% 
    dplyr::filter(query_name=={{query_name}}) %>% 
    dplyr::pull(uuid)
  
  
  # does the query has date_filter option as provided in the function
  query_type=api.queries.5[[name_conservation_area]] %>% 
    dplyr::filter(query_name=={{query_name}}) %>% 
    dplyr::pull(typeKey)
  
  
  #query with spatial information
  query_spatial=api.queries.5[[name_conservation_area]] %>% 
    dplyr::filter(query_name=={{query_name}}) %>% 
    dplyr::pull(spatial_query)
  
  
  if(!query_spatial &  type_output=="shp"){
    stop("Selected query does not have spatial information. To assess the
    queries in your conservation area has spatial data use the function
    query_info first and check the value for the 'spatial_query' feature")}
  
  #> query typeKey is important to learn if they are executable from
  #> Connect (based on connect functionality) and if there is spatial 
  #> info available -> If the .shp is an available option or not.
  # copy from semicolon -> ;view-source:https://karukinkaconnect.smartconservationtools.org/server/connect/query
  
  type_key_selected_query<-api.queries.5[[name_conservation_area]] %>% 
    dplyr:: filter(query_name=={{query_name}}) %>% 
    dplyr::pull(typeKey)
  
  if(!date_filter_types_available_per_query_type %>% 
     dplyr::filter(typeKey==type_key_selected_query) %>% 
     dplyr::select(contains(date_filter)) %>% 
     dplyr::pull()){
    
    stop("Selected query does not have the date_filter option provided. 
         Run the query_info function to assess the 
         values available for your query type. 
         To assess the query type, use the function
         'query_info' first and check the values of the 
         'date_filter_type:' features")}
  
  #----------------------------------------------#
  # create the api address of the specific query #
  #----------------------------------------------#
  
  #> api without start date or end date specified. UTM_zone is null or assigned 
  #> so the link will be fine
  

  api_address_minimal<-
    
    
    dplyr::case_when(
      type_output=="csv" ~
        paste0(server_url, 
               "/connect/query/api/", 
               query.api.number,
               "?format=",
               type_output, 
               "&date_filter=",
               date_filter)[1], 
      
      
      type_output=="shp" ~
        paste0(server_url, 
               "/connect/query/api/", 
               query.api.number,
               "?format=",
               type_output, 
               "&date_filter=",
               date_filter,
               "&srdi",
               srid,
               UTM_zone)[1])     
  
  
  
  # add the filter dates if provided
  
  start_date_full<-if(!is.null(start_date)){ #if null, it remains null
    paste0("&start_date=", 
           #stringr::str_glue("{lubridate::year(start_date)}-{lubridate::month(start_date)}-{lubridate::day(start_date)}"),
           paste(lubridate::year(start_date), lubridate::month(start_date), lubridate::day(start_date),  sep = "-"),
            "%2000%3A00%3A00")}
  
  end_date_full<-if(!is.null(end_date)){
    paste0("&end_date=", 
           #stringr::str_glue("{lubridate::year(end_date)}-{lubridate::month(end_date)}-{lubridate::day(end_date)}"), 
           paste(lubridate::year(start_date), lubridate::month(start_date), lubridate::day(start_date),  sep = "-"),
           "%2023%3A59%3A59")}
  
  
  api_address <- paste0(api_address_minimal,
                        start_date_full,
                        end_date_full)
  
  #---------------------------------------------#
  # go to the api address and download the data #
  #---------------------------------------------#
  
  data = rvest::session_jump_to(logged.in.connect, api_address)
  
  
  #open the query data as spatial data
  if(type_output=="shp"){
    filename<-stringr::str_extract(data$response$headers$`content-disposition`, "(?<=\\=).*")
    filename<-stringr::str_replace(filename, ".zip", replacement = paste0(".",type_output))
    data = dlshape(shploc=data$response$content,
                   shpfile=filename)
    data = data %>% janitor::clean_names()}
  
  #open the query data as tibble
  if(type_output=="csv"){
    
    data = data$response %>% rvest::read_html() %>% rvest::html_text()
    data = utils::read.csv(text=data, sep="," )
    data = data %>% janitor::clean_names()}
  
  return(data)}
