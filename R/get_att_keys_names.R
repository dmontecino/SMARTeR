library(xml2)

# the "path" parameter is the path to the conf model .xml file downloaded
# from SMART Desktop

get_atts_keys_names=function(path, language){
  
  doc<-read_xml(path)
  
  doc <- doc %>% xml_ns_strip()
  
  doc <- xml_find_all(doc, ".//node")
  
  
  # ------------------------------ #
  # Get the keys of the attributes #
  # ------------------------------ #
  
  # get the attribute keys per category
  attribute_keys_per_category<-map(doc, \(x){
    xml_find_all(x, ".//attribute") %>% 
      xml_attr("attributeKey")})
  
  attribute_keys<-unlist(attribute_keys_per_category, use.names = F)
  
  # ------------------------------- #
  # Get the names of the attributes #
  # ------------------------------- #
  
  #find the languages of the attributes. Each list object is 
  # a category in the configurable model.
  temp<-map(doc, \(x){xml_find_all(x, ".//name") %>% 
      xml_attr("language_code")})
  
  
  #indexes of the atttributes in the corresponding selected language
  #the first index shows the category name in the corresponding list object
  # so it is removed to leave only the attributes
  indexes_attribute_names_per_category<-map(temp, \(x) which(x == language)[-1])
  
  
  #get the attribute names in the corresponding language
  
  attribute_names_language<-
    map2(
      map(doc, \(x){xml_find_all(x, ".//name") %>% 
          xml_attr("value")}), # the list with the category and att names
      indexes_attribute_names_per_category, # list with indexes of the category names
      function(z,y) z[y]) # subset the category names in the corresponding language
  
  attribute_names_language<-unlist(attribute_names_language, use.names = F)
  
  
  # ---------------------------------------------------------------------- #
  # Create dataset with the keys for the categories and correspiding names #
  # ---------------------------------------------------------------------- #
  
  att_keys_names_conf_model=
    as_tibble(data.frame(att_key=attribute_keys,
                         att_name=attribute_names_language))
  
  
  return(att_keys_names_conf_model)}
