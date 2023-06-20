library(rvest)
library(tidyverse)
library(jsonlite)
library(janitor)
library(sf)

# ------------------------------------------------------------------------------------- #
# function to flatten the json to a single level and create dataset with the query data #
# ------------------------------------------------------------------------------------- #

flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}



# --------------------------------------------------------------------------------------------------------- #
# function to save the data as a temp, unzip it, and open it as a sf object when the type selected is "shp" #
# --------------------------------------------------------------------------------------------------------- #

dlshape=function(shploc, shpfile) { 
  temp=tempfile()  
  writeBin(shploc, temp)
  unzip(temp) 
  fp <- sf::read_sf(shpfile) 
  unzipped_files <- list.files(".", pattern = "\\.fix|\\.shp|\\.dbf|\\.shx|\\.prj", full.names = TRUE)
  file.remove(unzipped_files)
  return(fp)} 


# ----------------------------------#
# function to get data from connect #
# ----------------------------------#

# For now it only works for "PatrolQuery" and "PatrolObservationQuery" 
# but should be easy to solve for other types of queries.

data_from_connect<-function(server_url, 
                            user, 
                            password, 
                            name_conservation_area,
                            query_name,
                            type_output,
                            date_filter,
                            start_date=NULL, #YYYY-MM-DD
                            end_date=NULL, #YYYY-MM-DD
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
  
                         
  # -------------------------------------------------------------------------- #
  # go to Connect to find information about the requested query and run checks #
  # -------------------------------------------------------------------------- #
  
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
  
  # a flatten list of the data for the queries for each conservation area
  api.queries.3<-lapply(api.queries.2, flattenlist)
  
  # finding the information of queries per conservation area
  
  #> find elements with the key words "items" and "uuid" or "subFolders" and
  #> "items", and "uuids"
  api.queries.per.CA<-
    map(api.queries.3, \(x)
        x[grep("items.*uuid|subFolders.*items.*uuid", 
               names(x), 
               perl = TRUE)]) %>% 
    map(function(y) unlist(y, use.names = F))
  
  #> find elements with the key words "items" and "name" or "subFolders" and
  #> "items", and "name" 
  name.queries.per.CA<-
    map(api.queries.3, 
        \(x) x[grep("items.*name|subFolders.*items.*name", names(x))]) %>% 
    map(function(y) unlist(y, use.names = F))
  
  #> find elements with the key words "items" and "type" but not "typeKey" or
  #>"subFolders" and items", and "type" but not "typeKey"
  type.queries.per.CA<-
    map(api.queries.3, 
        \(x) x[grep("items.*typeKey|subFolders.*items.*typeKey", 
                    names(x), 
                    perl = TRUE)]) %>% 
    map(function(y) unlist(y, use.names = F))
  
  #queries available per conservation area in a dataset format
  api.queries.4<-
    
    pmap(list(api.queries.per.CA, 
              name.queries.per.CA,
              type.queries.per.CA),
         function(x,y,z) {data.frame(query_api=x, 
                                     query_name=y,
                                     query_type=z)})
  
  #checking if any query is available for any CA    
  if(all(sapply(sapply(api.queries.4, "[[", "query_name"), length)==0)){
    stop("there are no queries available in connect")}
  
  # assign the names of the conservation areas
  names(api.queries.4)<-names.conservation.areas
  
  
  #add information about executable queries
  api.queries.5<-map(api.queries.4,  ~ .x %>%
                       
       mutate(executable=query_type%in%
                c('patrolobservation', 'patrolquery', 'patrolwaypoint',
                  'patrolsummary', 'patrolgrid', 'observationobservation',
                  'observationwaypoint', 'observationsummary', 'observationgrid',
                  'entityobservation', 'entitywaypoint', 'entitysummary',
                  'entitygrid', 'surveyobservation', 'surveywaypoint',
                  'surveysummary', 'surveygrid', 'surveymission',
                  'surveymissiontrack', 'assetobservation', 'assetwaypoint',
                  'assetsummary', 'assetdeploymentsummary', 'i2_obs_query',
                  'i2_entity_summ_query', 'i2_entity_record_query', 
                  'i2_record_query', 'i2_record_summ_query')) %>% 
       
       # learn if the query has spatial information
       mutate(spatial_query=query_type%in%
                c("entityobservation", "entitywaypoint",
                  "intelligencerecord",  "surveymission",
                  "surveymissiontrack", "observationobservation", 
                  "observationwaypoint", 
                  "patrolobservation", "patrolquery", "patrolwaypoint", 
                  "surveyobservation", "surveywaypoint",
                  "assetobservation", "assetwaypoint")))

  
  # select the specific query uuid
  query.api.number<-api.queries.5[[name_conservation_area]] %>% 
    dplyr::filter(query_name=={{query_name}}) %>% 
    pull(query_api)
  
  
  # does the query has date_filter option as provided in the function
  query_type=api.queries.5[[name_conservation_area]] %>% 
    filter(query_name=={{query_name}}) %>% 
    pull(query_type)
  
  # executable query
  query_executable=api.queries.5[[name_conservation_area]] %>% 
    filter(query_name=={{query_name}}) %>% 
    pull(executable)
  
  #query with spatial infomration
  query_spatial=api.queries.5[[name_conservation_area]] %>% 
    filter(query_name=={{query_name}}) %>% 
    pull(spatial_query)
  
  
  #> query typeKey is important to learn if they are executable from
  #> Connect (based on connect functionality) and if there is spatial 
  #> info available -> If the .shp is an available option or not.
  # copy from semicolon -> ;view-source:https://karukinkaconnect.smartconservationtools.org/server/connect/query

  
  if(!query_executable){
    stop("Selected query is not executable by SMART Connect. To assess the
         queries in your conservation area executable by SMART Connect use the function
         queries_available_per_conservation_area first and check the 'executable'")}
    
  if(!query_spatial){
    stop("Selected query does not have spatial information. To assess the
    queries in your conservation area has spatial data by SMART Connect use the function
    queries_available_per_conservation_area first and check the 'spatial_query'
    column. In any case, You should get data if type_output='csv'")}
  
  
  #date_filter_per_query_type<-date_filters_types_available()
  source('R/query_type_date_filters_types_available.R')
  date_filter_per_query_type<-date_filters_types_available()
  
  if(!date_filter%in%date_filter_per_query_type[[query_type]]){
    stop("Selected query does not have the date_type option provided. 
         Run the date_filters_types_available() function to assess the 
         options available for yout query type. 
         To assess the query type, use the function
         queries_available_per_conservation_area first and check the 
         'query_type' column")}
  
  
  #----------------------------------------------#
  # create the api address of the specific query #
  #----------------------------------------------#
  
  #> api without start date or end date specified. UTM_zone is null or assigned 
  #> so the link will be fine
  
  api_address_minimal<-
    
    
    case_when(
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
                           str_glue("{year(start_date)}-{month(start_date)}-{day(start_date)}"),
                          "%2000%3A00%3A00")}
  
  end_date_full<-if(!is.null(end_date)){
                    paste0("&end_date=", 
                           str_glue("{year(end_date)}-{month(end_date)}-{day(end_date)}"), 
                           "%2023%3A59%3A59")}
                    
  
  api_address <- paste0(api_address_minimal,
                        start_date_full,
                        end_date_full)
  
  
  #---------------------------------------------#
  # go to the api address and download the data #
  #---------------------------------------------#

  data = session_jump_to(logged.in.connect, api_address)
                        
                      
  #open the query data as spatial data
  if(type_output=="shp"){
    filename<-stringr::str_extract(data$response$headers$`content-disposition`, "(?<=\\=).*")
    filename<-stringr::str_replace(filename, ".zip", replacement = paste0(".",type_output))
    data = dlshape(shploc=data$response$content,
                   shpfile=filename)}
  
  #open the query data as tibble
  if(type_output=="csv"){
    
    data = data$response %>% read_html() %>% html_text()
    data = read.csv(text=data, sep="," )
    data2 = data %>% janitor::clean_names()}
  
  return(data)}


# data_from_connect(server_url = "https://karukinkaconnect.smartconservationtools.org/server", 
#                             user = "dmontecino", 
#                             password = 
#                             name_conservation_area = "WCS Chile - Patrol Monitoring 1.0 [SMART]",
#                             query_name = "informacion_patrullas",
#                             type_output = "shp",
#                             date_filter="waypointlastmodified",
#                             start_date="2020-01-01", #YYYY-MM-DD
#                             end_date="2023-06-01", #YYYY-MM-DD
#                             srid=4326, 
#                             UTM_zone=NULL)
  
