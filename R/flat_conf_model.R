#' Flat table of a Configurable Model in a specified language
#' 
#' Function to create a flat table from a SMART Configurable Model downloaded 
#' from a Conservation Area in SMART Desktop exported as xml. The output has all
#' categories, attributes (list, multilist, trees, and others), and options. 
#' Keys and labels for each item are provided. All inactive categories, attributes,
#' and options are NOT included in the result.
#'
#' @param path_conf_model the path to the xml file ending in "/xml_file_name.xml"
#' @param language_interest a string code with the language of interest. If the
#' configurable model has more than one language, use this function once per 
#' language. English is the default language ("en"). If you want the English version
#' of the configurable model, jsut provide the path_conf_model. If the configurable
#' model also has Laotian language then provide the string "lo". For other languages
#' see: 
#' https://meta.wikimedia.org/wiki/Template:List_of_language_names_ordered_by_code 
#'
#' @return A tibble. The tibble has the following
#' columns: 
#' "cat_key", "cat_label", "att_type", "att_key", "att_label", 
#' "root_key", "root_label", "att_option_key", and "att_option_label". The "cat", 
#' "att", "root" strings in the column headers refer to Category, Attribute,
#' and Tree data, respectively. The "key", "label", and "option" strings refer to
#' data regarding the key, the label, and the options available, respectively.
#' The output only contains active Categories, Attributes, and Options. Keys do 
#' not change across languages.
#' 
#' @export
#'
#' @examples
#' flat_conf_model("your_path/xml_file_name.xml", language_interest)
#' 
#' @details
#' Currently, it is not possible to obtain the configurable model directly from
#' SMART Connect, so the Configurable Model must be exported from the Conservtion
#' Areas in SMART Desktop as an xml file to a known destination in your computer. 
#' Then provide the path to the xml file in your local computer in the 'path_conf_model'
#' attribute of this function. 
#' To export a Configurable Model, open SMART Desktop and the corresponding 
#' Conservation Area. Go to the Conservation Area tab and click on "Configurable 
#' Model". Select the Configurable Model you would like to present as a flat 
#' table from the list and then click on "Export". Choose the location to save the
#' xml file in your computer. Then run this function providing the corresponding 
#' path.
#' 
#' Tree attributes of the configurable model are assumed to have roots and options
#' ((two levels top).

# -----------------------------------------------------#
# function to get a configurable model as a flat table #
# -----------------------------------------------------#



flat_conf_model<-function(
    path_conf_model,
    language_interest="en"){


#open conf model xml path
conf_model<-xml2::read_xml(path_conf_model)

#if file is not an xml stop
if(!identical(class(conf_model), c("xml_document","xml_node"))){
  stop("file is not an xml")}

#clean node names
conf_model<-conf_model %>% xml2::xml_ns_strip()
#> in this step, the conf model misses the 
#> xmlns="http://www.smartconservationsoftware.org/xml/1.0/dataentry"> in the
#> very first line of xml code. Removing this line is necessary in the specific
#> case of conf models coming from SMART. If this is not done with the conf model
#> xml, the node paths are not identified. 


# -------------------------------------------- #
# Get the Categories of the Configurable Model #
# -------------------------------------------- #

category_nodes<-xml2::xml_find_all(conf_model, "//nodes/node")

if(length(category_nodes)==0){
  stop("No SMART Categories available in your Conservation Area. 
       No output expected")}

# divide nodes in list objects 
category_nodes<-split(category_nodes, seq_along(category_nodes))
  # purrr:::map(seq_along(category_nodes), function(y) category_nodes[y])


#category keys
cat_keys<-cat_ids <- purrr::map_vec(
    category_nodes, \(x) x %>% 
    xml2::xml_attr("categoryKey"))

#category labels
cat_labels<-purrr::map_vec(
    category_nodes, \(x) x %>%
    xml2::xml_find_all("name") %>% 
    xml2::xml_attrs() %>% 
      dplyr::bind_rows() %>% 
      dplyr::filter(language_code==language_interest) %>% 
      dplyr::pull(value))

#category ids
cat_ids <- purrr::map_vec(
    category_nodes, \(x) x %>% 
    xml2::xml_attr("id"))

#> If there are no labels that means the translaiton to the language of interest
#> is not available or the language of interest parameter is wrong

if(length(cat_labels)==0){
  stop("Language of interest id not available for your Categories
       or your 'language_interest' parameter is wrong")}

# if the cat vectors are not the same length then it means there is a problem
# with the available translations for them
if(!identical(length(cat_ids), length(cat_labels), length(cat_keys))){
  stop("Check that all your categories labels have been translated to the
       language of interest")}

#category data
cat_data<-tibble(cat_key=cat_keys,
                 cat_label=cat_labels,
                 cat_id=cat_ids)



# ------------------------#
# Attributes per Category #
# ----------------------- #

attributes_per_category<-purrr::map(category_nodes, \(x) x %>% 
                                    xml2::xml_find_all("attribute"))

#> if no category has attributes then create an empty dataset. This would work if there 
#> are categories but no attributes in any of them

if(all(purrr::map_vec(attributes_per_category, length)==0)){
  warning("Categories do not have active or inactive Attributes")

  att_data<- tibble(
    cat_key=rep(purrr::map_vec(category_nodes, function(x) x %>% 
                                 xml2::xml_attr("categoryKey")),
                purrr::map_vec(attributes_per_category, length)),
    att_key=character(),
    att_id=character(),
    att_label=character(),
    att_active=character(),
    att_type=character(),
    att_conf_id=character()
    )
  
  }else{

#> if at least one category has a single attribute then proceed to create the dataframe
#> of the attribute properties

    
# if at least one category does not have attributes, then warning
if(any(purrr::map_vec(attributes_per_category, length)==0)){
  warning("Some Categories do not have active or inactive Attributes")}

# leave data from Categories with attributes     
attributes_per_category_filtered<-attributes_per_category[
                                  purrr::map_lgl(attributes_per_category, \(x) 
                                                 length(x)>0)]
    
    
#attribute keys
att_keys<-
  purrr::map(attributes_per_category_filtered, 
    function(x) x %>%
      xml2::xml_attr("attributeKey")) %>%
  unlist(use.names = F)


#attribute active
att_active=
purrr::map(
  attributes_per_category_filtered, \(x) x %>%
    xml2::xml_find_all("option") %>% 
    xml2::xml_attrs() %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(id=="IS_VISIBLE") %>%
    dplyr::pull(doubleValue)) %>% 
    unlist(use.names = F)
 
#attribute labels
att_labels<-purrr::map(
  attributes_per_category_filtered, \(x) x %>%
    xml2::xml_find_all("name") %>% 
    xml2::xml_attrs() %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(language_code==language_interest) %>% 
    dplyr::pull(value)) %>% 
  unlist(use.names = F)



#> configuration of the attribute per category 
#> Points to the configuration of the attribute
#> currently used in each category using the same attribute
#> but i a different configuration
att_conf_ids<-purrr::map(
  attributes_per_category_filtered, \(x) x %>%
  xml2::xml_attr("configId")) %>%
  unlist(use.names = F)
  

# type of attribute per conf model
att_type<-purrr::map(
  attributes_per_category_filtered, \(x) x %>%
  xml2::xml_attr("type")) %>%
  unlist(use.names = F)

#>attribute id. this is the unique identifier of each attribute per category used.
#>so if the same attribute is used in more than one category, this id is different 
#> for the same attribute 

att_id<-purrr::map(
  attributes_per_category_filtered, \(x) x %>%
    xml2::xml_attr("configId")) %>%
  unlist(use.names = F)

if(!identical(length(att_keys), length(att_labels))){
  stop("The translation for the selected language is not available for all
       or none of the attributes")}

# create attribute data dataframe
 
  att_data<- tibble(
  cat_key=rep(purrr::map_vec(category_nodes, function(x) x %>% 
                   xml2::xml_attr("categoryKey")),
              purrr::map_vec(attributes_per_category, length)),
  att_key=att_keys,
  att_id=att_id,
  att_label=att_labels, 
  att_active=att_active,
  att_type=att_type, 
  att_conf_id=att_conf_ids) 

}  

# att_data %>% filter(att_key=="species_whn")




# ---------------------------------------#
# ----- list, multilist attributes-------# 
# ---------------------------------------#

#Find list multilist attributes

#> If there are no list or multilist attributes then dont search for their options
if(nrow(att_data)>0 && all(!grepl("LIST", att_data$att_type))){

  warning("No list or multilist attributes")
  
  # emty data frame for list or multilist attributes
  list_multilist_attributes_options <- tibble(
    att_key = character(),
    att_conf_id= character(),
    att_option_key = character(),
    att_option_label = character(),
    att_option_active = character()
  )}else{ # if there are list or multilist attributes

  # list_attributes_keys<-att_data %>% 
  #   dplyr::filter(grepl("LIST", att_type)) %>% 
  #   dplyr::pull(att_key) 
  # 
  
      
  list_attributes_conf_ids<-att_data %>%
    dplyr::filter(grepl("LIST", att_type)) %>%
    dplyr::pull(att_conf_id)
  
  attributes_conf_nodes<-conf_model %>% xml2::xml_find_all("//attributeConfig")
  
  #>index of the tree attribute node locations. Based on conf ids
  #>tha tells ehat configuraiton of the current attribute is being used in each 
  #>case. The options per list can be different even for the same attribute if 
  #>they have different configuraetions in the configuraetion model. So it is 
  #>to id the attribute and the confguration being used. From there, I can learn
  #>the options  for each instance that the attribute is used in different categories
  position_list_nodes<-
    purrr::map(list_attributes_conf_ids, function(x) 
      which(attributes_conf_nodes %>% 
              xml2::xml_attr("id")==x))
  
  # the list nodes
  list_nodes<-
    purrr::map(position_list_nodes, \(x) attributes_conf_nodes[x])

  
  att_key<-purrr::map(list_nodes, \(x) x %>% 
                        xml2::xml_attr("attributeKey"))
  
  att_conf_id<-purrr::map(list_nodes, \(x) x %>% 
                            xml2::xml_attr("id"))

  if(!identical(length(att_key), length(att_conf_id))){
    stop("The translation for the selected language is not available for all
       or none of the list or multilist attributes")}
  
  att_option_key<-
    purrr::map(list_nodes, \(x) x %>% 
    xml2::xml_find_all("listItem") %>% 
      xml2::xml_attrs() %>% 
      dplyr::bind_rows() %>% 
      dplyr::pull("keyRef"))
  
  if(length(unlist(att_conf_id))==0){
    warning("List and multilist attributes exists none of them have options")}
  
  if(any(purrr::map_vec(att_option_key, length)==0)){
    warning("At least one list or multilist attribute does not have options")}
  
  att_option_active<-
    purrr:: map(list_nodes, \(x) x %>% 
    xml2::xml_find_all("listItem") %>% 
      xml2::xml_attrs() %>% 
      dplyr::bind_rows() %>% 
      dplyr::pull("isActive"))
  
  att_option_label<-
    purrr::map(list_nodes, \(x) x %>% 
    xml2::xml_find_all("listItem") %>% 
    xml2::xml_find_all("name") %>% 
    xml2::xml_attrs() %>% 
      dplyr::bind_rows() %>% 
      dplyr::filter(language_code==language_interest) %>% 
      dplyr::pull(value))
  
  if(!identical(purrr::map_vec(att_option_active, length), 
                purrr::map_vec(att_option_label, length))){
    stop("The translation for the selected language is not available for all
       or none of the list/multilist attribute options")}
  
  list_multilist_attributes_options<-
  purrr::map(seq_along(list_nodes), \(x)
      dplyr::tibble(att_key=att_key[[x]],
             att_conf_id=att_conf_id[[x]],
             att_option_key=att_option_key[[x]],
             att_option_label=att_option_label[[x]],
             att_option_active=att_option_active[[x]])) %>% 
        dplyr::bind_rows()
  
  } # if there are list or multilist attributes
    



# ---------------------------------#
# ----- Find data for trees -------# 
# ---------------------------------#


#> If there are no tree attributes then dont search for their options
  if(nrow(att_data)>0 && all(!grepl("TREE", att_data$att_type))){
    
    warning("No tree attributes")
    
    # empty data frame for list or multilist attributes
    tree_att_root_options <- tibble(
      att_key = character(),
      att_conf_id = character(),
      root_key = character(),
      root_label = character(),
      root_active = character(),
      att_option_key = character(),
      att_option_label = character(),
      att_option_active = character())
    
}else{  #if there are tree attribuutes, get their data

  
  tree_attributes_conf_ids<-att_data %>%
    dplyr::filter(grepl("TREE", att_type)) %>%
    dplyr::pull(att_conf_id)
  
  attributes_conf_nodes<-conf_model %>% xml2::xml_find_all("//attributeConfig")
  
  position_tree_nodes<-
    purrr::map(tree_attributes_conf_ids, function(x) 
      which(attributes_conf_nodes %>% 
              xml2::xml_attr("id")==x))
  
  # the tree nodes
  tree_nodes<-
    purrr::map(position_tree_nodes, \(x) attributes_conf_nodes[x])  
  
  
  att_key<-purrr::map(tree_nodes, \(x) x %>% 
                        xml2::xml_attr("attributeKey"))
  
  att_conf_id<-purrr::map(tree_nodes, \(x) x %>% 
                            xml2::xml_attr("id"))
  
  if(!identical(length(att_key), length(att_conf_id))){
    stop("The translation for the selected language is not available for all
       or none of the tree attributes")}
  
  root_key<-purrr::map(tree_nodes, \(x) x[[1]] %>% 
                xml2::xml_find_all("treeNode") %>% 
                xml2::xml_attr("keyRef"))
  
  root_active<-purrr::map(tree_nodes, \(x) x[[1]] %>% 
                     xml2::xml_find_all("treeNode") %>% 
                     xml2::xml_attr("isActive"))
    
  root_label<-
    purrr::map(tree_nodes, \(x) x[[1]] %>% 
    xml2::xml_find_all("treeNode") %>% 
    xml2::xml_find_all("name") %>% 
    xml2::xml_attrs() %>% 
    dplyr::bind_rows() %>% 
      dplyr::filter(language_code==language_interest) %>% 
      dplyr::pull(value))
  
  #add warning
  
  if(length(unlist(root_key))==0){
    warning("Tree attributes exists none of them have roots")}

  if(any(purrr::map_vec(root_key, length)==0)){
    warning("At least one tree attribute does not have options")}

  if(!identical(purrr::map_vec(root_active, length), 
                purrr::map_vec(root_label, length))){
    stop("The translation for the selected language is not available for all
       or none of the tree roots")}
  
    dplyr::bind_rows()
  
    
    #create the data for the roots of the current tree attribute
    tree_att_root_options<-
      purrr::map(seq_along(tree_nodes), \(x)  
                  dplyr::tibble(att_key=att_key[[x]],
                                att_conf_id=att_conf_id[[x]],
                                root_key= root_key[[x]],
                                root_label=root_label[[x]],
                                 root_active=root_active[[x]], 
                                 att_option_key=NA, # root options colimuns to add the data in the next section
                                 att_option_label=NA, # root options colimuns to add the data in the next section
                                 att_option_active=NA)) %>% 
      dplyr::bind_rows() # root options colimuns to add the data in the next section

                  
    # --- Get the options for the yth root data of the current i tree attribute ---
    
    tree_nodes_roots<-
    purrr::map(tree_nodes, \(x) x[[1]] %>% 
                 xml2::xml_find_all("treeNode"))
    
    #>for each tree attribute split the roots in lists (each object is the nodeset of 
    #> an root of a tree
    #>
    tree_nodes_roots<-purrr::map(tree_nodes_roots, \(x) split(x, seq_along(x)))
                 
    
    root_options_key_temp<-
    purrr::map(tree_nodes_roots, \(x)
               purrr::map(x, \(y) y %>% 
                 xml2::xml_find_all("children") %>% 
                 xml2::xml_attr("keyRef")))
    
    
    root_options_active_temp<-
      purrr::map(tree_nodes_roots, \(x)
                 purrr::map(x, \(y) y %>% 
                              xml2::xml_find_all("children") %>% 
                              xml2::xml_attr("isActive")))
    
    root_options_label_temp<-
      purrr::map(tree_nodes_roots, \(x)
                 purrr::map(x, \(y) y %>% 
                              xml2::xml_find_all("children") %>% 
                              xml2::xml_find_all("name") %>% 
                              xml2::xml_attrs() %>% 
                              dplyr::bind_rows())) 
    
    root_options_label_temp<-purrr::map(root_options_label_temp, \(y)
                                 purrr::map(y, 
                                ~ if ("language_code" %in% colnames(.x)){
                                  dplyr::filter(.x, language_code == "en") %>% 
                                    dplyr::pull(value)}else as.character()))
    
    
    tree_att_root_options$att_option_key<-unlist(root_options_key_temp, recursive = F)
    tree_att_root_options$att_option_label<-unlist(root_options_label_temp, recursive = F)
    tree_att_root_options$att_option_active<-unlist(root_options_active_temp, recursive = F)
    
    

    # full tree attributes data frame 
  
  tree_att_root_options<-tree_att_root_options %>%
                         tidyr::unnest(
                         cols = c(att_option_key, att_option_label, att_option_active),
                         keep_empty = T)
                         
} # if thre are trees
  
  

# ---------------------------------------------- #
# Create the Dataset of the Conf Model Structure #
# ---------------------------------------------- #
  
  cat_att_data<-cat_data %>% 
    dplyr::left_join(att_data, by = c("cat_key"))
  
  
  #merge cat and att data with list multilist data
  cat_att_list_data<-cat_att_data %>% 
    dplyr::left_join(list_multilist_attributes_options %>% dplyr::bind_rows(), 
              by = c("att_key", "att_conf_id"), 
              relationship = "many-to-many")
  
  #merge cat, att data with tree data
  cat_att_tree_data=cat_att_data %>% 
    dplyr::left_join(tree_att_root_options, 
              by = c("att_key", "att_conf_id" )) 
  
  # join the datasets
  full_conf_model<-dplyr::full_join(cat_att_list_data, cat_att_tree_data)
          
  # filter for active atribtues, roots, and options
  full_conf_model<-
  full_conf_model %>% 
    dplyr::select(-cat_id, -att_id, -att_conf_id) %>% 
    dplyr::filter(is.na(att_active) | att_active !="0.0") %>%
    dplyr::filter(is.na(att_option_active) | att_option_active=="true") %>%
    dplyr::filter(is.na(root_active) | root_active=="true") %>%
    dplyr::select(cat_key, cat_label,
           att_type, att_key, att_label, -att_active,
           root_key, root_label, -root_active,
           att_option_key, att_option_label, -att_option_active)

return(full_conf_model)
}


# out<-flat_conf_model(path_conf_model=path_conf_model, 
#                 language_interest=language_interest)


