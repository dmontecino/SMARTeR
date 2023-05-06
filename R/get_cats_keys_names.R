library(xml2)

get_cats_keys_names <- function(path, language){

doc <-read_xml(path)
  
doc <- doc %>% xml_ns_strip()

doc <- xml_find_all(doc, ".//node")


# ------------------------------- #
# Get the keys of the categories  #
# ------------------------------- #

# get the category keys per category
category_keys_per_category<-map(doc, \(x){xml_attr(x, "categoryKey")})

category_keys<-unlist(category_keys_per_category, use.names = F)

# -------------------------------------------------------- #
# Get the names of the categories in the selected language #
# -------------------------------------------------------- #

#find the languages of the categories. Each list object is 
# a category in the configurable model.
temp<-map(doc, \(x){xml_find_all(x, ".//name") %>% 
                        xml_attr("language_code")})
        

#indexes of the category names in the corresponding selected language
#the first index shows the category name in the corresonding list object
#so select it
indexes_category_names_per_category<-map(temp, \(x) which(x == language)[1])


#get the category names in the corresponding language
category_names_language<-
map2(
map(doc, \(x){xml_find_all(x, ".//name") %>% 
               xml_attr("value")}), # the list with the category names
indexes_category_names_per_category, # list with indexes of the category names
function(z,y) z[y]) # subset the category names in the corresponding language

category_names_language<-unlist(category_names_language, use.names = F)


# ----------------------------------------------------------------------- #
# Create dataset with the keys for the categories and corresponding names #
# ----------------------------------------------------------------------- #

category_keys_names_conf_model=
as_tibble(data.frame(cat_key=category_keys,
                     cat_name=category_names_language))


return(category_keys_names_conf_model)}
