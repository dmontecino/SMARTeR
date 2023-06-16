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
                            type_output){
  
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
        \(x) x[grep("items.*type(?!Key)|subFolders.*items.*type(?!Key)", 
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
  
  
  # select the specific query uuid
  query.api.number<-api.queries.4[[name_conservation_area]] %>% 
    dplyr::filter(query_name=={{query_name}}) %>% 
    pull(query_api)
  
  
  # defining the query type of the query requested through the query name
  query_type=api.queries.4[[name_conservation_area]] %>% 
    filter(query_name=={{query_name}}) %>% 
    pull(query_type)
  
  query.type=
    case_when(
      query_type == "PatrolQuery" ~ "patrolstart",
      query_type == "PatrolObservationQuery" ~ "waypointdate",
      query_type =="ObsObservationQuery" ~ "waypointdate",
      .default = as.character(query_type)
    )
  
  
  #go to the api address of the specific query 
  data = session_jump_to(logged.in.connect, 
                         paste0(server_url, 
                                "/connect/query/api/", 
                                query.api.number,
                                "?format=",
                                type_output, 
                                "&date_filter=",
                                query.type)[1]) #query type for api call
  
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





