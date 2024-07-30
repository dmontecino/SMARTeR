#' Flat table of a Configurable Model in a specified language
#' 
#' Function to create a flat table from a SMART Configurable Model downloaded 
#' from a Conservation Area in SMART Desktop exported as xml. The output has all
#' categories, attributes (list, multilist, trees, and others), and options. 
#' Keys and labels for each item are provided. All inactive categories, attributes,
#' and options are NOT included in the result.
#'
#' @param path_conf_model the path to the xml file ending in "/xml_file_name.xml"
#' @param language_interest a string code with the language of interest. "en" 
#' is the default value. If configurable model has more than one language, use 
#' this function once per language. The value for this argument must be consistent
#' with one of the languages available for the configurable model.
#' @param only_active a boolean to choose if the flat table should include only 
#' active attributes, tree roots, and options (TRUE) or everything (FALSE)
#'
#' @return A tibble. The tibble has the following
#' columns: 
#' "cat_key_level_n", "cat_label_level_n", "att_key", "att_label", "att_type",
#' "att_active", "root_key", "root_label", "root_active", "option_key", and 
#' "option_label", "option_active". The "cat", "att", "root", and "option" 
#' strings in the column headers refer to Category, Attribute, Tree and Option 
#' data, respectively. The "key", "label", and "option" strings refer to data 
#' regarding the keys and the labels, The "level_n" refers to the 
#' level of the nested Category. Keys do not change across languages.
#' 
#' @export
#'
#' @examples
#' path_conf_model <- system.file("extdata", "example_configurable_model.xml", package = "SMARTeR")
#' 
#' flat_conf_model(
#' path_conf_model=path_conf_model, 
#' language_interest="en",
#' only_active=TRUE)
#' 
#' @details
#' Currently, it is not possible to obtain the configurable model directly from
#' SMART Connect, so the Configurable Model must be exported from the Conservation
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
#' For now, tree attributes of the configurable model are assumed to have roots 
#' and options (two levels top). Otherwise you will get a wrong output for tree
#' attributes.

# -----------------------------------------------------#
# function to get a configurable model as a flat table #
# -----------------------------------------------------#

flat_conf_model<-function(
    path_conf_model,
    language_interest="en",
    only_active=TRUE){
  
  
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
  
  
  #### #### #### #### #### #### #### 
  #### GET DATA FROM CATEGORIES ####
  #### #### #### #### #### #### #### 
  
  category_nodes<-xml2::xml_find_all(conf_model, "//nodes/node")
  # category_nodes<-xml2::xml_find_all(conf_model,"//*[@language_code]")
  # category_nodes %>% xml2::xml_attr("language_code")
  
  if(length(category_nodes)==0){
    stop("No SMART Categories available in your Conservation Area. 
       No output expected")}
  
  
  ## LABELS
  
  cat_hkeys <- xml2::xml_find_all(conf_model, "//*[@categoryHkey]")
  cat_hkeys<-cat_hkeys %>% xml2::xml_attr("categoryHkey")
  
  #>nodes of all categories in the conf model including those nested in other categories
  #>no matter the number of nested levels
  nodeset<-
    lapply(seq_along(cat_hkeys), function(k) 
      xml2::xml_find_all(conf_model, paste0(".//*[@categoryHkey[contains(., '", cat_hkeys[k], "')]]")))
  
  
  #> list to store the names pf the parent nodes through the category levels
  result<-vector(mode = "list", length = length(nodeset))
  
  
  for(i in seq_along(nodeset)){
    
    #> get the number of parent nodes per nodeset  
    num_parent_nodes<-
      length(nodeset[[i]] %>% 
               xml2::xml_parents() %>% 
               xml2::xml_name() %>% 
               subset(. =="node"))
    
    #if ther are no parents, then the only value is 0
    my_list <- lapply(0:num_parent_nodes, function(x) 0:x)
    
    for(z in seq_along(my_list)){
      
      #if the only value is 0 (no parents)
      if(all(my_list[[z]]==0)){ 
        
        #the use the same nodeset
        x <-  nodeset[[i]]
        
        # and get the name and values
        # out<-xml2::xml_child(x, "name") %>% 
        #      xml2::xml_attr("value")
        out <- xml2::xml_find_all(x, "name") %>% 
          xml2::xml_attrs() %>% 
          dplyr::bind_rows() %>% 
          dplyr::filter(language_code==language_interest) %>% 
          dplyr::pull(value)
        
        #if the node has parents
      }else{
        
        #then get the nodeset    
        x <-  nodeset[[i]]  
        
        #>but this time find the parent of of the parent and get the 
        #>name and value attribute of each one
        for (y in my_list[[z]][-1]) {
          x <- xml2::xml_parent(x)}
        
        out<-xml2::xml_find_all(x,"name") %>% 
          xml2::xml_attrs() %>% 
          dplyr::bind_rows() %>% 
          dplyr::filter(language_code==language_interest) %>% 
          dplyr::pull(value)
      }
      
      #> If 'out' is nothing, that means the translation to the language of interest
      #> is not available or the language of interest parameter is wrong 
      if(length(out)==0){
        stop("Language of interest id not available for your Categories
       or your 'language_interest' parameter is wrong")}
      
      #> store the value for all parents of each node
      result[[i]][z]<-out
      
    }
    
  }
  
  #> labels as tibble
  cat_labels<-purrr::map_dfr(purrr::map(result, rev), 
                             ~ as.data.frame(t(.x))) %>% 
    dplyr::as_tibble()
  
  #> rename the cat_labels tibble
  names(cat_labels)<-paste0("cat_label_level_", seq(ncol(cat_labels)))
  
  
  
  ## KEYS
  
  cat_keys<-
    purrr::map(cat_hkeys, \(y)
        dplyr::tibble(V = 
                        purrr::map(y, \(x) strsplit(x = x, split = "[.]")[[1]]))  %>% 
          tidyr::unnest_wider(V, names_sep = ""))
  
  
  cat_keys<-dplyr::bind_rows(cat_keys) 
  
  names(cat_keys)<-paste0("cat_key_level_", seq(ncol(cat_keys)))
  
  
  
  
  #### #### #### #### #### #### #### 
  #### GET DATA FROM ATTRIBUTES ####
  #### #### #### #### #### #### #### 
  
  attribute_data<-purrr::map(nodeset, \(x) 
                             dplyr::tibble(
                               
                               att_key=  
                                 x %>% 
                                 xml2::xml_find_all("attribute") %>% 
                                 xml2::xml_attr("attributeKey"),
                               
                               att_label=
                                 x %>% 
                                 xml2::xml_find_all("attribute/name") %>% 
                                 xml2::xml_attr("value") %>% 
                                 magrittr::extract(x %>% 
                                                     xml2::xml_find_all("attribute/name") %>% 
                                                     xml2::xml_attr("language_code")==language_interest),
                               
                               att_type=
                                 x %>% 
                                 xml2::xml_find_all("attribute") %>% 
                                 xml2::xml_attr("type"),
                               
                               att_config_id=
                                 x %>% 
                                 xml2::xml_find_all("attribute") %>% 
                                 xml2::xml_attr("configId"),
                               
                               att_active=
                                 x %>% 
                                 xml2::xml_find_all("attribute/option") %>% 
                                 xml2::xml_attrs() %>% 
                                 dplyr::bind_rows() %>% 
                                 dplyr::filter(id=="IS_VISIBLE") %>%
                                 dplyr::pull(doubleValue) %>% 
                                 unlist(use.names = F)))
  
  #> if no category has attributes then warning
  
  if(dplyr::bind_rows(attribute_data) %>% nrow()==0){
    warning("Categories do not have active or inactive Attributes")}
  
  
  #### #### #### #### #### #### #### #### #### #### #### #### 
  #### ADD OPTIONS OF LIST MULTILIST AND TREE ATTRIBUTES ####
  #### #### #### #### #### #### #### #### #### #### #### #### 
  
  # ---------------------------------------#
  # ----- list, multilist attributes-------# 
  # ---------------------------------------#
  
  
  list_mlist_att_data<-purrr::map(attribute_data, \(x) 
                                  x %>% dplyr::filter(grepl("LIST", att_type)))
  
  list_mlist_option_data<-vector(mode = "list", 
                                 length = length(list_mlist_att_data))
  
  for(i in seq_along(list_mlist_att_data)){
    
    if(nrow(list_mlist_att_data[[i]])>0){ # if there are list or mlist attributes
      
      list_mlist_option_data[[i]]<-vector(mode = "list", length = nrow(list_mlist_att_data[[i]]))
      
      for(y in seq(nrow(list_mlist_att_data[[i]]))){
        
        list_mlist_option_data[[i]][[y]]<-
          
          #list options keys
          
          #if there are options then build the tibble
          
          if(length(xml2::xml_find_all(conf_model, 
                                       paste0(".//*[@id[contains(., '", 
                                              list_mlist_att_data[[i]]$att_config_id[y], "')]]")) %>% 
                    xml2::xml_find_all(".//listItem"))>0){
          dplyr::tibble(
            
            att_config_id=    
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        list_mlist_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_attr("id"),  
            
            # if there are options for the list, then get them, otherwise NA
            
            option_key=    
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        list_mlist_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//listItem")  %>% 
              xml2::xml_attr("keyRef"),
            
            
            #list options labels
            option_label=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        list_mlist_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//listItem")  %>% 
              xml2::xml_find_all("name") %>% 
              xml2::xml_attrs() %>% 
              dplyr::bind_rows() %>% 
              dplyr::filter(language_code==language_interest) %>% 
              dplyr::pull(value),
            
            #list if option is active
            option_active=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        list_mlist_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//listItem")  %>% 
              xml2::xml_attr("isActive"))
          
          }else{ # if there are not options 
            list_mlist_option_data[[i]][[y]]<-dplyr::tibble(
              att_config_id=NA,
              option_key= NA, 
              option_label=NA,
              option_active=NA)}
      }
    }else{
      
      list_mlist_option_data[[i]]<-dplyr::tibble(
        att_config_id=NA,
        option_key= NA, 
        option_label=NA,
        option_active=NA)}
    
  }
  
  if(dplyr::bind_rows(list_mlist_option_data) %>% nrow()==0){
    warning("No list or multilist attributes")}
  
  
  # ---------------------------------#
  # ----- Find data for trees -------# 
  # ---------------------------------#
  
  
  tree_att_data<-purrr::map(attribute_data, \(x) x %>% 
                              dplyr::filter(grepl("TREE", att_type)))
  
  
  # <treeNode keyRef="anual" hkeyRef="anual." dmUuid="98f61d01f54b4726ac6a8dfda270d4eb" isActive="true" id="82de9907-83e8-482f-b849-2eed5e294f6b" isCustomImage="false">
  #   <name language_code="es" value="ANUAL" source="DM"/>
  #   <children keyRef="caadeazucar" hkeyRef="anual.caadeazucar." dmUuid="115e08d22a8245fc8ab13500f46f0489" isActive="true" id="d8ae2049-bc8e-40e4-a767-ea081a04e5c6" isCustomImage="false">
  #   <name language_code="es" value="Caña de azucar" source="DM"/>
  #   </children>
  
  
  tree_root_option_data<-vector(mode = "list", length = length(tree_att_data))
  
  for(i in seq_along(tree_att_data)){
    
    if(nrow(tree_att_data[[i]])>0){
      
      tree_root_option_data[[i]]<-vector(mode = "list", length = nrow(tree_att_data[[i]]))
      
      for(y in seq(nrow(tree_att_data[[i]]))){
        
        tree_root_option_data[[i]][[y]]<-
          
          # if the tree has options then proceed
          if(length(xml2::xml_find_all(conf_model, 
                                       paste0(".//*[@id[contains(., '", 
                                              tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
                    xml2::xml_find_all(".//treeNode"))>0){
          
          
          #list options keys
          dplyr::tibble(
            
            att_config_id=    
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_attr("id"),  
            
            #root keys
            root_key=                
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode")  %>% 
              xml2::xml_attr("keyRef"),
            
            #root labels
            root_label=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode/name")  %>% 
              xml2::xml_attrs() %>% 
              dplyr::bind_rows() %>% 
              dplyr::filter(language_code==language_interest) %>% 
              dplyr::pull(value),
            
            # root active
            root_active=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode")  %>% 
              xml2::xml_attr("isActive"),
            
            
            #option keys
            option_key=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode")  %>% 
              purrr::map(\(x) x %>% 
                           xml2::xml_find_all(".//children") %>% 
                           xml2::xml_attr("keyRef")) %>% 
              purrr::map(\(x) if(length(x)==0){"NA"}else{x}),
            
            
            #option labels
            option_label=
              # xml2::xml_find_all(conf_model, 
              #                    paste0(".//*[@id[contains(., '", 
              #                           tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              #   xml2::xml_find_all(".//treeNode")  %>% 
              #   purrr::map(\(x) x %>% 
              #         xml2::xml_find_all(".//children/name") %>% 
              #         xml2::xml_attrs() %>% 
              #         dplyr::bind_rows() %>% 
              #         ifelse(nrow(x)>0,
              #         dplyr::filter(language_code==language_interest) %>% 
              #         dplyr::pull(value), NA)),
            
            xml2::xml_find_all(conf_model, 
                               paste0(".//*[@id[contains(., '", 
                                      tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode")  %>% 
              purrr::map(\(x) {
                children <- xml2::xml_find_all(x, ".//children/name") %>% 
                  xml2::xml_attrs() %>% 
                  dplyr::bind_rows()
                
                if (nrow(children) > 0) {
                  result <- children %>%
                    dplyr::filter(language_code == language_interest) %>% 
                    dplyr::pull(value)
                } else {
                  result <- "NA"#as.character()
                }
                
                return(result)
              }),
            
            
            #option active
            option_active=
              xml2::xml_find_all(conf_model, 
                                 paste0(".//*[@id[contains(., '", 
                                        tree_att_data[[i]]$att_config_id[y], "')]]")) %>% 
              xml2::xml_find_all(".//treeNode")  %>% 
              purrr::map(\(x) x %>% 
                           xml2::xml_find_all(".//children") %>% 
                           xml2::xml_attr("isActive")) %>% 
              purrr::map(\(x) if(length(x)==0){"NA"}else{x})
            
            
            
          )
        
          }else{
            tree_root_option_data[[i]][[y]]<-dplyr::tibble(
              att_config_id=NA,
              root_key=NA,
              root_label=NA,
              root_active=NA,
              option_key= NA, 
              option_label=NA,
              option_active=NA)
          }
      }
    }else{
      
      tree_root_option_data[[i]]<-dplyr::tibble(
        att_config_id=NA,
        root_key=NA,
        root_label=NA,
        root_active=NA,
        option_key= NA, 
        option_label=NA,
        option_active=NA)}
    
  }
  
  # Create full dataset
  
  
  tree_list_mlist<-
    purrr::map2(
      purrr::map(tree_root_option_data, \(y) y %>% 
            dplyr::bind_rows() %>% 
            tidyr::unnest(c(option_key, option_label, option_active))), 
      purrr::map(list_mlist_option_data, \(x) x %>% dplyr::bind_rows()),
      dplyr::bind_rows)
  
  
  
  attribute_list_mlist_data<-vector(mode = "list", length = length(nodeset))
  
  for(i in seq_along(nodeset)){
    attribute_list_mlist_data[[i]]<-  
      dplyr::left_join(
        attribute_data[[i]],
        tree_list_mlist[[i]],
        by = "att_config_id", 
        relationship = "many-to-many") %>% dplyr::distinct()}
  
  
  full<-
    dplyr::bind_cols(cat_keys,cat_labels) %>% 
    dplyr::select(matches("_level_")) %>% 
    dplyr::mutate(list=attribute_list_mlist_data) %>% 
    tidyr::unnest(list) %>% 
    dplyr::select(-att_config_id) 
  

  if(only_active==TRUE){
    full <- full %>% dplyr::filter(att_active%in%c("1.0", "2.0")) 
    full <- full %>% dplyr::filter(!(att_type=="MLIST" & option_active=="false"))
    full <- full %>% dplyr::filter(!(att_type=="LIST" & option_active=="false"))  
    full <- full %>% dplyr::filter(!(att_type=="TREE" & root_active=="false"))  
  }
  
  return(full)}
  
} # end of function

