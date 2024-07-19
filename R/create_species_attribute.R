#' Create species attribute for SMART Desktop 7's Data Models
#' 
#' Function to create a data model that only has a species attribute as a tree.
#' Then this data model with just the species tree can be merged to the Conservation
#' Areas' data model and it becomes available to be added to the Categories. Do not
#' use non-ASCII characters in the function arguments!
#' 
#' @param language The abbreviation of the language of the attribute as a string.
#' It must be compatible with the it has to be consistent with the languages 
#' available in the data model in SMART Desktop. If your Conservation Area has 
#' as language "english" and "laotian", then the argument 'language' can be 
#' either "en" or "lo" but not "fr" (french). To find the abbreviations check 
#' the options in your Conservation Area Data Model on the top-left corner and 
#' click on the downward arrow.
#' @param species_attribute_key A string that will become the key of the species 
#' attribute. It should not have weird characters. The function takes care of 
#' uppercase letters and white spaces. 
#' @param species_in_language A string that will become the label of the tree 
#' attribute
#' @param first_tax_level A character vector to provide the keys and labels of 
#' the fist taxonomic level of the species attribute. 
#' @param species_key A character vector to provide the keys of the species within 
#' each taxonomic group of the first level of the tree. Vector elements should not
#' have weird characters. The function takes care of uppercase letters and white 
#' spaces.
#' @param species_label A character vector to provide the labels of the species 
#' within each taxonomic group of the first level of the tree (e.g., common local
#' names).
#' @param path the path to the folder to save the species attribute-only data model
#'
#' @return
#' xml file named "species_smarter_attribute.xml" saved in your working directory.
#' This is the "Data Model" with just the species attribute to be merged to the
#' Conservation Area's Data model and get the species attribute available.
#' 
#' @details
#' Basically the function 'create_species_function' creates a Data Model that only  
#' has a species attribute as a tree with two taxonomic levels. Then this species  
#' attribute-only data model can be merged into the user's Data Model and the 
#' attribute becomes available to be added to the Categories. The species tree 
#' attribute can be flattened to a list in SMART Desktop.
#' The output of the function is an xml file named "species_smarter_attribute.xml"
#' saved in your working directory. To add the attribute to your data model, open  
#' the Conservation Area, open the Data Model, find the "Merge Data Model" button 
#' in the bottom left and select the "species_smarter_attribute.xml" file. Add the 
#' attribute to the Category or Categories (search for the attribute usig the key
#' provided in 'species_attribute_key').
#' 
#' @export
#'
#' @examples
#' first_tax_level<-c(rep('Aves', 3), rep("Mammalia", 3))
#' 
#' species_key=c('Circus melanoleucos', 
#'               'Spilornis cheela', 
#'               'Dendrocygna javanica', 
#'               'Bos gaurus', 
#'               'Bos javanicus', 
#'               'Capricornis milneedwardsii maritimus')
#'               
#' species_label=c('Pied harrier',
#'   'Crested serpent eagle',
#'    'Lesser whistling duck',
#'    'Wild water buffalo',
#'    'Red muntjac',
#'    'Large-antlered Muntjac')
#'    
#' path <- tempdir()  
#' 
#' create_species_attribute(
#' language="en", 
#' species_attribute_key="species_smarter",
#' species_in_language="species",
#' first_tax_level=first_tax_level,
#' species_key=species_key,
#' species_label=species_label,
#' path=path)


create_species_attribute<-
  function(language="en", 
           species_attribute_key="species_smarter",
           species_in_language="species",
           first_tax_level,
           species_key,
           species_label,
           path){
           

    
#sheets<-1:number_of_sheets
#languages<-c("en", translated_language)
species_attribute_key<-c("species_smarter") # key or label?

#open data
#df <- map(sheets, \(x) read_excel(path = path, sheet = x))
df <- tibble::tibble(first_tax_level=first_tax_level,
             species_key=species_key,
             species_label=species_label)

#split the data by first_tax_level
sheets<-split(df, df$first_tax_level)


#Start new .xml
doc <- xml2::xml_new_root("DataModel", 
          "xmlns"="http://www.smartconservationsoftware.org/xml/1.0/datamodel") 

# add firsts child node "languages
xml2::xml_add_child(doc, "languages")


#add language(s) of the data model 
for(i in language){
  doc %>% 
    xml2::xml_child("languages") %>% 
    xml2::xml_add_child("languages") %>%  
    xml2::xml_set_attr("code", i)}



#add the species attribute which is a tree.
# the first part of the tree are the taxonomic first_tax_leveles
# and the second level correspond to the species
# within

xml2::xml_add_child(doc, "attributes")

doc %>%  
  xml2::xml_child("attributes") %>% 
  xml2::xml_add_child("attribute")

doc%>% 
  xml2::xml_child("attributes/attribute") %>% 
  xml2::xml_set_attrs(c(
    "key"=gsub("[^a-zA-Z]", "", species_attribute_key),
    "isrequired"="true",
    "type"="TREE"))

# add the languages for the species attribute
#for(i in seq_along(language)){
  doc %>%
    xml2::xml_child("attributes/attribute") %>% 
    xml2::xml_add_child("names") %>% 
    xml2::xml_set_attrs(c(
      "language_code"=language,
      "value"=species_in_language))#}


# add a child for each taxonomic first_tax_level and create the corresponding attributes
# add first_tax_leveles keys

for(i in seq(sheets)){
  doc %>% 
    xml2::xml_child("attributes/attribute") %>%
    xml2::xml_add_child("tree") %>%
    xml2::xml_set_attrs(c(key=tolower(unique(sheets[[i]][,"first_tax_level"])), 
                          isactive="1"))} #add first_tax_leveles keys


# #add the species keys in each first_tax_level as children nodes
for(i in seq(sheets)){
  for(y in 1:nrow(sheets[[i]])){

    doc %>% 
      xml2::xml_child("attributes/attribute")%>%
      xml2::xml_child(i + length(language)) %>%
      xml2::xml_add_child("children") %>%
      xml2::xml_set_attrs(c(key=gsub("\\s+", "_", 
                                     tolower(sheets[[i]][y,"species_key"])),
                          isactive="1"))}}


#add the names of the species in each language 
for(i in seq(sheets)){ # species nodes in each first_tax_level
  temp_species<-doc %>%
    xml2::xml_child("attributes/attribute")%>%
    xml2::xml_child(i + length(language)) %>%
    xml2::xml_find_all(".//children")


  for(y in 1:length(temp_species)){ #within each species

      value<-stringr::str_to_sentence(gsub("\\u2019", 
                                           "", 
                                           sheets[[i]][y,"species_label"]))
                    
      temp_species[[y]] %>%
        xml2::xml_add_child("names") %>%
        xml2::xml_set_attrs(c(language_code=language,
                              value=value))
  }}
  





# Generate a temporary file name to save the current xml file
temp_file <- tempfile(fileext = ".xml")

# Save the XML document to the temporary file
xml2::write_xml(doc, temp_file)

# Read the XML content from the temporary file in another format
xml_text <- readLines(temp_file)

#edit the declaration of the xml
xml_text[1]<-'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'

#save the xml with the data model that contains only a species attribute
file_path <- file.path(path, "species_smarter_attribute.xml")

writeLines(xml_text, con = file_path)

# Remove the temporary file
file.remove(temp_file)

  }
