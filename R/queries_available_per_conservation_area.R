
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
  
  #queries available per conservation area 
  api.queries.4<-
    
    lapply(api.queries.3, function(x)  
      # data.frame(
      #   query_api=unlist(unname(x[grepl(".uuid", names(x), ignore.case = F)])),
      #   query_name=unlist(unname(x[which(grepl(".uuid", names(x), ignore.case = F))+1])),
      #   query_type=unlist(unname(x[which(grepl(".uuid", names(x), ignore.case = F))+2]))))
      data.frame(
        query_api=x$items.uuid,
        query_name=x$items.name,
        query_type=x$items.type))
      
      
  if(all(sapply(sapply(api.queries.4, "[[", "query_name"), length)==0)){
    stop("there are no queries available in connect")}

  # assign the names of the conservation areas
  names(api.queries.4)<-names.conservation.areas
  
  # see all query data for all the conservation areas that have at least one query available
  return(api.queries.4)}
  
