# Function to creata a table that shows all categories, attributes (list, multilist, trees, and others) and their corresponding options in the configurable model. 
# The output also shows the keys of each option, attribute, and category, as well as the labels for the configurable model. 
# The output does not include options or attributes inactivated at the datamodel or configurable model levels. So it is basically a summary of what SMART Mobile users will 
# see in their screens. 
# Tree attributes in the configurable model are assumed to have two levels only. 


library(xml2)
library(tidyverse)
library(purrr)
library(stringr)

path_conf_model<-"/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/SMART_FOR_HEALTH_2023/Rangers/Configurable_Model_June_2023_Obs_Spec.xml"


#open conf model xml path
conf_model<-read_xml(path_conf_model)

#clean node names
conf_model<-conf_model%>% xml_ns_strip()
# in this step I miss xmlns="http://www.smartconservationsoftware.org/xml/1.0/dataentry"> in the
# very first line of text


# -----------#
# Categories #
# ---------- #

categories<-xml_find_all(conf_model, "//nodes/node")

categories<-map(seq_along(categories), function(y) categories[y])



# Category data

cat_key<-unlist(map(categories, function(x) x %>% xml_attr("categoryKey")))

cat_label<-unlist( 
map2(
#list of names across all languages in the conf model  
map(categories, function(x) x %>% xml_find_all("name") %>% xml_attr("value")),
  
#index with the position of the name in english  
map(categories, function(x) x %>% xml_find_all("name")) %>% 
  map(function(y) y %>% xml_attr("language_code")) %>% 
  map(function(z) z %>%  str_detect("en")), 

#subset the first list based on the posiiton of the english laguage
function(x,y) subset(x,y)))

cat_data<-
  data.frame(cat_key=cat_key,
             cat_label=cat_label)



# ------------------------#
# Attributes per Category #
# ----------------------- #

all_attributes_per_category_conf_model_nodes<-
map(seq_along(categories), 
    function(x) categories[[x]] %>% 
      xml_find_all("attribute"))


all_attributes_keys_per_category_conf_model<-
map(all_attributes_per_category_conf_model_nodes, 
    function(x) x %>%
      xml_attr("attributeKey"))


# Disregard attributes because they are not available in the actual configuration
# attribute - option id="IS_VISIBLE" doubleValue="1.0"/>

all_attributes_activation_per_category<-
map(seq_along(all_attributes_keys_per_category_conf_model),  
    function(index){

map2(
#indexes of the "IS VISIBLE" id for each attribute for each category
map(all_attributes_per_category_conf_model_nodes[[index]],
    function(x) x %>%  
            xml_find_all("option") %>% 
            xml_attr("id") %>% 
            str_detect("IS_VISIBLE")),

#activation value (0.0 means deactivated)
map(all_attributes_per_category_conf_model_nodes[[index]],
    function(x) x %>% 
            xml_find_all("option") %>% 
            xml_attr("doubleValue")), 

function(list1, list2) list2[list1])
      
})




all_attribute_labels_per_category_conf_model<-
map2(
  #list of names across all languages in the conf model  
  map(all_attributes_per_category_conf_model_nodes, 
      function(x) x %>% xml_find_all("name")) %>% 
    map(function(y) y %>% xml_attr("value")),
  
  #index with the position of the name in english  
  map(all_attributes_per_category_conf_model_nodes,
    function(x) x %>% xml_find_all("name")) %>% 
    map(function(y) y %>% xml_attr("language_code")) %>% 
    map(function(z) z %>%  str_detect("en")), 
  
  #subset the first list based on the posiiton of the english laguage
  function(x,y) subset(x,y))


all_attribute_ids_per_category_conf_model<-
  map(all_attributes_per_category_conf_model_nodes, 
      function(x) x %>% xml_attr("configId"))

all_attribute_types_per_category_conf_model<-
  map(all_attributes_per_category_conf_model_nodes, 
      function(x) x %>% xml_attr("type"))

# create attribute data dataframe

att_data<-
data.frame(
  cat_key=unlist(map2(map(categories, function(x) x %>% xml_attr("categoryKey")),
               map(all_attributes_keys_per_category_conf_model, length), 
               function(x,y) rep(x,y))),
  att_key=unlist(all_attributes_keys_per_category_conf_model),
  att_label=unlist(all_attribute_labels_per_category_conf_model),
  att_active=unlist(all_attributes_activation_per_category),
  att_type=unlist(all_attribute_types_per_category_conf_model),
  att_conf_id=unlist(all_attribute_ids_per_category_conf_model))
    

# ----------------------#
# Options per Attribute #
# --------------------- #

# ----- Find options for list, multilist -------# 

categories2<-xml_find_all(conf_model, "//attributeConfig")

all_ids_by_conf_attribute<-map(seq_along(categories2), function(x) categories2[[x]])

all_conf_attributes_keys<-map(all_ids_by_conf_attribute, function(x) x %>% xml_attr("attributeKey"))

all_conf_attributes_ids<-map(all_ids_by_conf_attribute, function(x) x %>% xml_attr("id"))

all_conf_options<-
map2(
map(all_ids_by_conf_attribute, function(x) 
  x %>% 
    xml_find_all("listItem/name") %>% 
    xml_attr("value")),

map(all_ids_by_conf_attribute, function(x) 
  x %>% 
  xml_find_all("listItem/name")%>%
  xml_attr("language_code") %>%
  str_detect("en")), 

function(list1, list2) list1[list2])


all_conf_options_keys<-
  map(all_ids_by_conf_attribute, 
      function(x) 
        x %>% xml_find_all("listItem") %>% 
        xml_attr("keyRef"))


all_conf_options_active<-
map(all_ids_by_conf_attribute, 
    function(x) 
      x %>% xml_find_all("listItem") %>% 
      xml_attr("isActive"))


# Create the dataframe for multilist and list attributes
list_multilist_attributes_options <- 
      data.frame(att_key = unlist(rep(all_conf_attributes_keys, lengths(all_conf_options))),
                 att_conf_id = unlist(rep(all_conf_attributes_ids, lengths(all_conf_options))),
                 att_option_key=unlist(all_conf_options_keys),
                 att_option_label=unlist(all_conf_options),
                 att_option_active=unlist(all_conf_options_active))



#removing those unused configurations for specific attributes
list_multilist_attributes_options<-
  list_multilist_attributes_options %>%  
  filter(att_conf_id%in%att_data$att_conf_id) # rows with att_id not in the attribute data id are gone



# ----- Find options for trees -------# 

#At this point only tree attributes should be missing their options
# this function works only for trees with 2 levels: the root
# and the options per root

attributes_conf_nodes<-conf_model %>% xml_find_all("attributeConfig")

attributes_nodes<-conf_model %>% xml_find_all("//attribute")


tree_attributes_keys<-
  subset(
    attributes_nodes %>% xml_attr("attributeKey"),
    attributes_nodes %>% xml_attr("type")=="TREE")


position_tree_conf_attributes<-
map(tree_attributes_keys, function(x) 
  which(attributes_conf_nodes %>% xml_attr("attributeKey")==x))



attributes_tree_conf_nodes<-
  attributes_conf_nodes[unlist(position_tree_conf_attributes)]


#data from configurable model nodes (roots)

tree_roots_attributes_conf_model_data<-
  
data.frame(
  
att_key=unlist(map(attributes_tree_conf_nodes, function(x) x  %>% xml_attr("attributeKey"))),

att_option_key=unlist(map(attributes_tree_conf_nodes, function(x) x %>% xml_find_all("treeNode") %>% xml_attr("keyRef"))),

att_option_active=unlist(map(attributes_tree_conf_nodes, function(x) x  %>% xml_find_all("treeNode") %>%  xml_attr("isActive"))),

att_option_label=
    unlist(map2(
    map(attributes_tree_conf_nodes, function(x) x %>% xml_find_all("treeNode/name") %>% xml_attr("value")),
    map(attributes_tree_conf_nodes, function(x) x %>% xml_find_all("treeNode/name") %>% xml_attr("language_code")=="en"),
    function(x,y) subset(x,y)))
)




root_key<- 
  map(split(attributes_tree_conf_nodes, seq_along(attributes_tree_conf_nodes)), 
      function(x) map(x %>% xml_find_all("treeNode"), 
                      function(y) y %>% 
                        xml_attr("keyRef")))
 
tree_option_keys<-
map(split(attributes_tree_conf_nodes, seq_along(attributes_tree_conf_nodes)), 
    function(x) map(x %>% xml_find_all("treeNode"), 
      function(y) y %>% 
      xml_find_all("children")%>% 
      xml_attr("keyRef")))


all_tree_options_labels<-map(split(attributes_tree_conf_nodes, seq_along(attributes_tree_conf_nodes)), 
    function(x) map(x %>% xml_find_all("treeNode"), 
                    function(y) y %>% 
                      xml_find_all("children/name")%>% 
                      xml_attr("value")))

index_tree_options_labels_english<-map(split(attributes_tree_conf_nodes, seq_along(attributes_tree_conf_nodes)), 
        function(x) map(x %>% xml_find_all("treeNode"), 
                        function(y) y %>% 
                          xml_find_all("children/name")%>% 
                          xml_attr("language_code")=="en"))

tree_option_labels<-
map(seq_along(all_tree_options_labels), 
    function(i) 
      map2(all_tree_options_labels[[i]], 
           index_tree_options_labels_english[[i]], 
           function(x,y) subset(x,y)))


tree_option_active<-
  map(split(attributes_tree_conf_nodes, seq_along(attributes_tree_conf_nodes)), 
      function(x) map(x %>% xml_find_all("treeNode"), 
                      function(y) y %>% 
                        xml_find_all("children")%>% 
                        xml_attr("isActive")))

# Create the dataframe for tree attributes
tree_options_conf_model_data<-

data.frame(
  att_option_key=unlist(
             map2(root_key, 
                  tree_option_keys, 
                  function(x, y) 
                    rep(x, lengths(y)))),
  tree_option_key=unlist(tree_option_keys, use.names = F),
  tree_option_label=unlist(tree_option_labels, use.names = F),
  tree_option_active=unlist(tree_option_active, use.names = F))



# ---------------------------------------------- #
# Create the Dataset of the Conf Model Structure #
# ---------------------------------------------- #


# Merge the datasets

#tree roosts and options

temp<-tree_roots_attributes_conf_model_data %>% left_join(tree_options_conf_model_data, by = "att_option_key")


# trees and attribute data

temp2<-att_data %>% right_join(temp, by = "att_key")


# list multilist and attribute data

temp3<-att_data %>% right_join(list_multilist_attributes_options, by = c("att_key", "att_conf_id"))
# head(temp3)
# temp3


# all attribute data
all_att_data<-temp2 %>% full_join(temp3)
# head(all_att_data)



# all data

# head(cat_data)
all_conf_data<-cat_data %>% left_join(all_att_data, by="cat_key")

#finally remove everything that is not active

all_conf_data_active=all_conf_data %>% 
  filter(att_active!="0.0" | is.na(att_active)) %>% 
  filter(att_option_active!="false" | is.na(att_option_active)) %>% 
  filter(tree_option_active!="false" | is.na(tree_option_active)) %>% 
  select(-c(att_conf_id, att_active, att_option_active, tree_option_active))

# write.csv(all_conf_data_active, "")
# 
# temp=
# all_conf_data_active %>% 
#   select(cat_label, att_label, att_option_label, tree_option_label)
# 
# write.csv(temp, "")




