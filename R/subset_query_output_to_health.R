## To subset the query output by the health categories (in observation_category column)
## and health attributes (query columns)
## get the paramaters needed using the functions in

# source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/queries_available_per_conservation_area.R")
# source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/data_from_connect.R")
# source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/get_att_keys_names.R")
# source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/get_cats_keys_names.R")

#then

# queries_available_per_conservation_area( server_url = "your_server", 
#                                          user = "your_connectuser", 
#                                          password = "your_connect_password")

#data from the selected query located in the selected Conservation area in SMART
# query_output<-
#   data_from_connect(
#     name_conservation_area = "conservation_area_name",
#     query_name = "query_name",
#     type = "csv_or_shp",
#     server_url = "your_server", 
#     user = "your_connectuser", 
#     password = "your_connect_password")


#health cats
# cat_key_and_names<-get_cats_keys_names(
#   path = "your_path_conf_model.xml",
#   language="en") # or another one in the translations. Useful if the output
#                  # of the query is in another language set in the translations
                   # of the data model generating the conf model

#health atts
# att_key_and_names<-get_atts_keys_names(
#   path = "your_path_conf_model.xml",
#   language="en") # or another one in the translations. Useful if the columns
#                  # of the query output are in another language set in the 
                   # translations of the data model generating the conf model 

library(tidyverse)

# Actual function to subset data query to health data

subset_smart_query_data_to_health_cats_and_atts<-
  function(query_output, # as called by the data_from_connect.R function
           cat_key_and_names, # as called by the get_cats_keys_names.R
           att_key_and_names){ # as called by get_att_keys_names.R

#selecting the categories of the health component in the conf model
health_cats_conf_model<-cat_key_and_names %>% 
  filter(grepl("whn", cat_key)) %>% 
  pull(cat_name) %>% 
  unique()
  
  
  
#selecting the attributes of the health component in the conf model
health_atts_conf_model<-att_key_and_names %>% 
  filter(grepl("whn", att_key)) %>% 
  pull(att_name) %>% 
  unique() %>% 
  make_clean_names()
  

health_dat<-
  query_output %>% 
  filter(observation_category_0 %in% health_cats_conf_model) %>% # selecting health categories
  select(source, # selecting attributes that come with the query and are relevant
         waypoint_id, 
         waypoint_date,
         waypoint_time,  
         x, y,
         comment, last_modified, 
         last_modified_by,
         observation_category_0, 
         observation_group, 
         
         #select the health attributes 
         one_of(health_atts_conf_model[
           health_atts_conf_model %in% names(query_output)]))
         

return(health_dat)}

#subset_smart_query_data_to_health_cats_and_atts(
#           query_output=query_output, # as called by the data_from_connect.R function
#           cat_key_and_names=cat_key_and_names, #as called by the get_cats_keys_names.R
#           att_key_and_names=att_key_and_names){ # as called by get_att_keys_names.R
    


