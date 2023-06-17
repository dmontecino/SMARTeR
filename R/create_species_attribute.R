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
# https://github.com/dmontecino/SMARTeR/blob/main/wildlife_species_list_template.xlsx
# the function allows to translate the list of species to a desire language as well following the translations provided in the template above.
# Once AI translation tools become more available worldwide (such as deepl), I will create a function to translate species.
# the output is a xml file, that can be merge with a data model:
# Once the file is built go to Conservatin Area - Data Model - Merge Data Model. 
# Add the attribute named species_smarter


create_data_model_with_translated_species_attribute<-
  function(path, # path to the excel document with the species
           number_of_sheets, # number of sheets in the excel document. Each sheet represents a taxonomy class.
           translated_language=NULL, # if you just want to create a species attribute with the english names given in the .xlsx file, then leave it as NULL
           species_in_translated_language=NULL){ # spell "species" in the language of interest (NULL if english)

    
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

#Start new .xml
doc <- xml_new_root("DataModel", 
                    "xmlns"="http://www.smartconservationsoftware.org/xml/1.0/datamodel") 

# add firsts child node "languages
xml_add_child(doc, "languages")


#add language(s) of the data model 
for(i in languages){
  doc %>% xml_child("languages") %>% 
    xml_add_child("languages") %>%  
    xml_set_attr("code", i)}



#add the species attribute which is a tree.
# the first part of the tree are the taxonomic classes
# and the second level correspond tothe species
# within

xml_add_child(doc, "attributes")
doc %>%  xml_child("attributes") %>% xml_add_child("attribute")
doc%>% xml_child("attributes/attribute") %>% 
  xml_set_attrs(c(
    "key"="species_smarter",
    "isrequired"="true",
    "type"="TREE"))


# add the languages for the species attribute
for(i in seq_along(languages)){
  doc %>%
    xml_child("attributes/attribute") %>% 
    xml_add_child("names") %>% 
    xml_set_attrs(c(
      "language_code"=languages[i],
      "value"=species_attribute_name[i]))}


# add a child for each tanoxmic class and crreate the corresponding attributes
for(i in sheets){
  doc %>% xml_child("attributes/attribute") %>% 
    xml_add_child("tree") %>% 
    xml_set_attrs(c(key=tolower(unique(english_name[[i]]$class)), isactive="1"))} #add classes keys



#add the node "names" and add the names of the taxonmic classes 
#in english and translated languages

for(i in sheets){
  for(y in seq_along(languages)){
    
    value<-ifelse(languages[y]=="en", 
                  unique(english_name[[i]]$class),
                  unique(translated_name[[i]]$class_translated))       
    
    doc %>% xml_child("attributes/attribute")%>% 
      xml_child(i + length(languages)) %>% 
      xml_add_child("names") %>% 
      xml_set_attrs(c(language_code=languages[y],
                      value=value))}}




#add the species keys in each class as children nodes
for(i in sheets){
  for(y in 1:nrow(english_name[[i]])){
    
    doc %>% xml_child("attributes/attribute")%>% 
      xml_child(i + length(languages)) %>% 
      xml_add_child("children") %>% 
      xml_set_attrs(c(key=gsub(" ", "_", tolower(english_name[[i]]$species_scientific_name[y])),
                      isactive="1"))}}


#add the names of the species in each language 
for(i in sheets){ # species nodes in each class
  temp_species<-doc %>% 
    xml_child("attributes/attribute")%>% 
    xml_child(i + length(languages)) %>% 
    xml_find_all(".//children") 
  
  
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
