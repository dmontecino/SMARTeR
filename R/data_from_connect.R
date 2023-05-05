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
                            type){
  
    #open connect. The login page
    session_connect = session(server_url)
    
    #provide the username and password
    form.unfilled = session_connect %>% html_node("form") %>% html_form()
    
    form.filled = form.unfilled %>%
      html_form_set("j_username" = user,
                    "j_password" = password)
    
    #login
    logged.in.connect = session_submit(session_connect, form.filled) 
    
    # access the data of the queries available with their unique identifier
    api.queries = session_jump_to(logged.in.connect, paste0(server_url, "/api/query/tree"))
    
    if(length( api.queries$response %>% read_html())==0){
      stop("there are no conservation areas available in connect")}
    
    api.queries = api.queries$response %>% read_html() %>% html_text %>% fromJSON(simplifyVector = F)
    
    #names of the conservation areas hold by connect with queries available
    names.conservation.areas=sapply(api.queries, function(x) x$name)
    
    # a flatten list of the data for the queries for each conservation area with queries available in connect
    api.queries=lapply(api.queries, flattenlist)
    
    #queries available per conservation area
    api.queries=
      
      lapply(api.queries, function(x)  
        data.frame(
          query_api=unlist(unname(x[grepl(".uuid", names(x), ignore.case = F)])),
          query_name=unlist(unname(x[which(grepl(".uuid", names(x), ignore.case = F))+1])),
          query_type=unlist(unname(x[which(grepl(".uuid", names(x), ignore.case = F))+2]))))
    
   
    if(all(sapply(sapply(api.queries, "[[", "query_name"), length)==0)){
      stop("there are no queries available in connect")}
    
     names(api.queries)=names.conservation.areas
   
   
    # select the specific query uuid
    query.name=query_name
    
    query.api.number=api.queries[[name_conservation_area]] %>% 
      filter(query_name==query.name) %>% 
      pull(query_api)
    
    
    # defining the query type of the query requested through the query name
    query.type=api.queries[[name_conservation_area]] %>% 
      filter(query_name==query.name) %>% 
      pull(query_type)
    
    query.type=
      case_when(
        query.type == "PatrolQuery" ~ "patrolstart",
        query.type == "PatrolObservationQuery" ~ "waypointdate",
        query.type =="ObsObservationQuery" ~ "waypointdate",
        .default = as.character(query.type)
      )
    
    
    #go to the api address of the specific query 
    data = session_jump_to(logged.in.connect, 
                           paste0(server_url, 
                                  "/connect/query/api/", 
                                  query.api.number,
                                  "?format=",
                                  type,
                                  "&date_filter=",
                                  query.type))
    
    #open the query data as spatial data
    if(type=="shp"){
    data = dlshape(shploc=data$response$content, 
                   shpfile=paste0(strsplit(data$response$headers$`content-disposition`, ";|[.]|=")[[1]][3], ".shp"))}
    
    #open the query data as tibble
    if(type=="csv"){
    
    data = data$response %>% read_html() %>% html_text()
    data = read.csv(text=data, sep="," )
    data = data %>% janitor::clean_names()}
    
    return(data)}
