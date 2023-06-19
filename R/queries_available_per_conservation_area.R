
library(rvest)
library(tidyverse)
library(jsonlite)

# --------------------------------------------- #
# function to flatten a list to a single level  #
# --------------------------------------------- #

flattenlist <- function(x){  
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){ 
      Recall(out)
    }else{
      return(out)
    }
  }



# ------------------------------------------------------------------------#
# function to check the conservation areas in connect you have access to  #
# and the corresponding queries within that conservation area             #
# ------------------------------------------------------------------------#

queries_available_per_conservation_area<-function(

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
    mutate(spatial_query=query_type%in%
          c("entityobservation", "entitywaypoint","intelligencerecord",  "surveymission",
            "surveymissiontrack", "observationobservation", "observationwaypoint", 
            "patrolobservation", "patrolquery", "patrolwaypoint", 
            "surveyobservation", "surveywaypoint",
            "assetobservation", "assetwaypoint")))
  
   # assign the names of the conservation areas
  names(api.queries.5)<-names.conservation.areas
  
  # see all query data for all the conservation areas that have at least one query available
  return(api.queries.4)}
  
