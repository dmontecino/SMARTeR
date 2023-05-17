library(xml2)
library(readxl)
library(tidyverse)
library(zoo)
library(stringr)

# function to create a data model that only has a species attribute as a tree attribute.
# the first node of the tree are the taxonomy classes and within the nodes
# are the names of the species. The data model keys are the scientific name whilst
# the data model labels are the species common name. 
# The list of species has to be provided following the format here: 

# the function allows to translate the list of species to a desire language as well


create_data_model_with_translated_species_attribute<-
  function(path, # path to the excel document with the species
           number_of_sheets, # number of sheets in the excel document. Each sheet represents a taxonomy class.
           translated_language=NULL, # if you just want to create a species
           #attribute with english names, then just leave it as NULL
           species_in_translated_language=NULL){ # how do you spell "species" in the translated language

    
sheets<-1:number_of_sheets
languages<-c("en", translated_language)
species_attribute_name<-c("species", species_in_translated_language)

#open data
df <- map(sheets, \(x) read_excel(path = path, sheet = x))



# modify dataset to replace na's with the previous string 
df<-map(df, function(x) {
  for(i in 1:ncol(x)){
    x[,i] <- zoo::na.locf(x[,i])}
  return(x)
})

# get the data of the species in english and in the translated language
english_name<-map(df, function(x) x %>% select(class, common_name, species_scientific_name))

translated_name<-map(df, function(x) x %>% 
                      select(grep("class_|common_name_", colnames(x))))




# Create baseline XML
xml <- read_xml('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<DataModel xmlns="http://www.smartconservationsoftware.org/xml/1.0/datamodel">
  <languages>
  </languages>
  <attributes>
    <attribute key="species_smarter" isrequired="true" type="TREE">
    </attribute>
  </attributes>
</DataModel>')


#add language(s) of the data model with the species attribute
children <- xml_children(xml)

for(i in languages){
children[[1]] %>% 
  xml_add_child("language") %>%  
  xml_set_attr("code", i)}


#add names of the species_smarter attribute (the key is defined in the xml = species_smarter)
for(i in 1:length(languages)){
  
children[[2]] %>% 
  xml_children() %>% 
  xml_add_child("names") %>% 
    xml_set_attrs(c(language_code=languages[i], 
                    value=species_attribute_name[i]))}
  


#add the classes of the species_smart attribute. the attribute is a tree.
# the first part of the tree are the classes and then the corresponding species
# within

for(i in sheets){
children[[2]] %>% 
  xml_children() %>% 
  xml_add_child("tree") %>% 
  xml_set_attrs(c(key=tolower(unique(english_name[[i]]$class)), isactive="1"))} #add classes keys


#add the names to each class in english and translated language

classes<-
children[[2]] %>% 
  xml_children() %>% 
  xml_children()

for(i in sheets){
  for(y in seq_along(languages)){

value<-ifelse(languages[y]=="en", 
             unique(english_name[[i]]$class),
             unique(translated_name[[i]]$class_translated))       
    
classes[[i+2]] %>% 
    xml_add_child("names") %>% 
    xml_set_attrs(c(language_code=languages[y],
                    value=value))}}



#add the species keys in each class as children nodes

for(i in sheets){
  for(y in 1:nrow(english_name[[i]])){

classes[[i+2]]  %>% 
    xml_add_child("children") %>% 
    xml_set_attrs(c(key=gsub(" ", "_", tolower(english_name[[i]]$species_scientific_name[y])),
                    isactive="1"))}}








#add the names of the species in each language 

for(i in sheets){ # species nodes in each class
temp_species<-classes[[i+2]] %>% xml_find_all(".//children") 


for(y in 1:length(temp_species)){ #within each species
 
  for(z in seq_along(languages)){ #add each language

  value<-ifelse(languages[z]=="en",  #the species name in the corresponding language
               str_to_sentence(gsub("’", "", english_name[[i]]$common_name)[y]),
               translated_name[[i]]$common_name_translated[y])       
    
  temp_species[[y]] %>% 
  xml_add_child("names") %>% 
  xml_set_attrs(c(language_code=languages[z],
                  value=value))
  }}}

return(xml)

  }

# run function

#path<-"/your_path.xlsx"
#number_of_sheets<-4
#translated_language="lo"
#species_in_translated_language="ຊະນິດ"

# species_translated<-create_data_model_with_translated_species_attribute(
#   path=path, 
#   number_of_sheets=number_of_sheets,
#   translated_language=translated_language,
#   species_in_translated_language=species_in_translated_language)