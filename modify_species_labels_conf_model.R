library(xml2)
library(readxl)
library(tidyverse)
library(zoo)
library(stringr)
library(googl)


# 
# 
# create_conf_model_with_translated_species_attribute<-
#   function(path,
#            number_of_sheets,
#            translated_language=NULL, # if you just want to create a species
#            #attribute with english names, then just leave it as NULL
#            species_in_translated_language){


path_conf_model<-"/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/LAO PDR/configurable_model_test.xml"
path_data<-"/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/LAO PDR/wildlife_species_list_english_lao_may_2023.xlsx"
number_of_sheets<-4
sheets<-1:number_of_sheets
language_code<-"lo" #the language to modify the species label in the conf model



#open species data
df <- map(sheets, \(x) read_excel(path = path_data, sheet = x))

# modify dataset to replace na's with the previous string in the column
# taxonomical class (e.g., mammalia) is merged across rows containing 
# mammalia species in the corresponding "species" row.

df<-map(df, function(x) {
  for(i in 1:ncol(x)){
    x[,i] <- zoo::na.locf(x[,i])}
  return(x)
})

# get the data of the species in english and in the translated language
english_name<-map(df, function(x) x %>% 
                    select(class, 
                           common_name, 
                           species_scientific_name))


translated_name<-map(df, function(x) x %>% 
                       select(grep("class_|common_name_", colnames(x))))









#open conf model xml path
conf_model<-read_xml(path_conf_model)

#clean node names
conf_model<-conf_model%>% xml_ns_strip()
# in this step I miss xmlns="http://www.smartconservationsoftware.org/xml/1.0/dataentry"> in the
# very first line of text

conf_model %>% xml_contents()

#identify the attributeConfig node that contains the species_smarter attribute
attributeKey_nodes<-xml_find_all(conf_model, "//attributeConfig")

attributeKey_names<-attributeKey_nodes %>%  xml_attr("attributeKey")

species_smarter_index<-grep("species_smarter", attributeKey_names)


#edit the label of each species in the species_smarter attribute 
#in the CONFIGURABLE MODEL ONLY. 
# to do this, manipulate the "attributeConfig-treeNode-children" nodes. Specifically,
# the "treeNode" nodes are the classes, and the "children" nodes are the species
# within. For each "children" node, edit the "name" node of the language to be modified
# (only one of them or all of them, in this case, just for lao [ the second "name" node])
# in each name node to modify, edit value atribute and the source attribute.

species_smarter_node=attributeKey_nodes[[species_smarter_index]] %>% xml_find_all("//treeNode")



for(i in sheets){
  
  class_species_conf_model<-species_smarter_node[[i]] %>% xml_children()
  
  #identify the "children" (species) nodes of each class
  index_class_species_to_modify<-
    which(!grepl("name", xml_name(class_species_conf_model)))
  
  for(y in index_class_species_to_modify){
    
    class_species_language_conf_model=class_species_conf_model[[y]]
    
    #identify the name of each "children" (species) in the species attribute as shown 
    # in the configurable model for the desired language. Identify the nodes containing
    # the species label in the conf model for the desired language
    
    index_node_to_modify=
      grep(language_code, 
           class_species_language_conf_model %>% 
             xml_children() %>% 
             xml_attr("language_code"))
    
    class_species_language_specific_conf_model=
      class_species_language_conf_model %>% 
      xml_child(search = index_node_to_modify)
    
    #name attribute index for all species
    
    number_languages=max(c(1:max(index_class_species_to_modify))[-index_class_species_to_modify])
    
    #Modify the value and surce attributes
    
    class_species_language_specific_conf_model %>% 
      xml_set_attr("value", #edit the value attribute
                   paste(translated_name[[i]]$common_name_translated[y-number_languages], 
                         str_to_sentence(english_name[[i]]$common_name[y-number_languages]),
                         sep="_"))
    
    #common name translated_common name english
    
    class_species_language_specific_conf_model %>% xml_set_attr("source", "CM")
    
  }}


#edit the label of each species in the species_smarter attribute 
#in the CONFIGURABLE MODEL ONLY. 
# to do this, manipulate the "attributeConfig-treeNode-children" nodes. Specifically,
# the "treeNode" nodes are the classes, and the "children" nodes are the species
# within. For each "children" node, edit the "name" node of the language to be modified
# (only one of them or all of them, in this case, just for lao [ the second "name" node])
# in each name node to modify, edit value atribute and the source attribute.

species_smarter_node=attributeKey_nodes[[species_smarter_index]] %>% xml_find_all("//treeNode")



# ns <- xml_ns(conf_model)
# # xml_ns_rename(ns, "xmlns")
# ns <- xml_ns_rename(ns, d1 = "xmlns")

# temp2<-conf_model %>% xml_find_all("//attributeConfig/treeNode/children") 
# temp2[[3]]
# temp[[3]]





#adding the "http://www.smartconservationsoftware.org/xml/1.0/dataentry" back
# conf_model %>% xml_find_all("/ConfigurableModel") %>% 
#   xml_set_attr("xmlns", "http://www.smartconservationsoftware.org/xml/1.0/dataentry")


xml <- read_xml('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                <ConfigurableModel xmlns="http://www.smartconservationsoftware.org/xml/1.0/dataentry" instantGps="false" photoFirst="false" iconSet="color"></ConfigurableModel>')

# Extract nodes from the edited conf-model document
extracted_nodes <- xml_find_all(conf_model, "//languages|name|nodes|attributeConfig|extraData")  # the edited nodes of the original conf model

# extracted_nodes[[1]] %>% xml_child()

for(i in 1:length(extracted_nodes)){
  
  xml %>% xml_add_child(extracted_nodes[[i]])}




# temp=conf_model %>% xml_find_all("//attributeConfig/treeNode/children")
# temp[[2]]
# 
# temp=xml %>% xml_find_all("//attributeConfig/treeNode/children")
# temp[[2]]


write_xml(xml, 
          "/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/LAO PDR/conf_model_edited.xml")


# cat(as.character( xml), "\n")
# 
#xml_structure(species_translated)
# xml_view_trees(xml)




