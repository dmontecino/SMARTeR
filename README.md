# SMARTeR 

A set of functions to support your work with the Spatial Monitoring and 
Reporting Tools version 7 (SMART; https://smartconservationtools.org/) Conservation Areas,
including a preparing a species tree attribute from a csv and load it to a Data Model,
run a query from SMART Connect and load the output in your R session, and convert
a Configurable model into a flat table with the corresponding Categories, Attributes,
and options (for multilist, lists, and tree attributes).


The package uses the dplyr (>= 1.1.2), janitor (>= 2.2.0), jsonlite (>= 1.8.5),
lubridate (>= 1.9.2), magrittr (>= 2.0.3), purrr (>= 1.0.1), rvest (>= 1.0.3),
sf (>= 1.0.13), stringr (>= 1.5.0), tibble (>= 3.2.1), tidyr (>= 1.3.0), 
xml2 (>= 1.3.4), zoo (>= 1.8.12) packages. 

# To run a query in SMART Connect and load the output in R:

First, call the function "query_info". This function has as arguments "server_url",
"user", and "password". Provide the connect url in the server_url argument as 
string (e.g, "https://wcshealth.smartconservationtools.org/server") then your 
username and password as string in the corresponding arguments. 
This function will return a hierarchical list. Each list element at the first 
level corresponds to each Conservation Area you have access that have at least
one query. Each list element at the first level is named based on the corresponding
Conservation Area. Within each list element there are listed two column tibbles. 
Each listed tibble has the information of the corresponding query. This information
is key to properly pass arguments to the 'data_from_connect' function and be able
load the output of a specific query (see next paragraph). More details in the 
function documentation.

Secondly, use the function "data_from_connect". This function also has as arguments
"server_url", "user", and "password", plus "name_conservation_area", "query_name",
"type_output", "date_filter", "start_date", "end_date", "srid", and "UTM_zone".
For the "name_conservation_area" argument provide the name of the conservation 
area holding the query of interest. This is the name of the list object at the
first level returned by the "query_info" function. The "query_name" is the name
of the listed tibble within the corresponding conservation area list object in the
output of the "query_info" function. queries_available_per_conservation_area". 
The "type_output" options are "shp" or "csv" for now. If the selection is csv, 
a tibble is created. If the selection is shp, a .zip file is saved as a temporary
file, unzipped, and read as an sf object. The "date_filter" is the reference day
to filter the query output by a "start_date" and "end_date". Not all queries types
have spatial information and not all query types have all date filters available.
The 'query_info' option gives you this information for each query. Finally, 
'srid' is the spatial projection of the spatial data and if you are using UTM, you
also have to provide the UTM zone in the argument "UTM_zone".

I suggest to use these functions to load output from patrol queries, 
observation queries, etc, but not summary queries. Summary queries will be loaded
but the row names as seen in SMART Desktop query output will be returned as an 
unnamed column. Also, only the first column header is read as the column names,
while any secondary column headers are read as data. So, I suggest to create an 
observation query that returns all the data, load it in R, and start creating 
summaries, stats, etc here. Summary queries are already summarized data so what's
the point.


```
query_info(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password")

```

and I get something like this: 

```
$`WCS Chile - Patrol Monitoring 1.0 [SMART]`
$`WCS Chile - Patrol Monitoring 1.0 [SMART]`$`2 Numero de Patrullas por Sector`
# A tibble: 14 × 2
   feature                               value                                                          
   <chr>                                 <chr>                                                          
 1 folder                                WCS Chile - Patrol Monitoring 1.0 [SMART]/Tests Diego (no usar)
 2 query_name                            2 Numero de Patrullas por Sector                               
 3 typeKey                               patrolsummary                                                  
 4 uuid                                  649ef5cd-90df-475a-896e-5bc4e7d4aa2e                           
 5 spatial_query                         FALSE                                                          
 6 date_filter_type:assetdeploymentdate  FALSE                                                          
 7 date_filter_type:missionenddate       FALSE                                                          
 8 date_filter_type:missionstartdate     FALSE                                                          
 9 date_filter_type:missiontrackdate     FALSE                                                          
10 date_filter_type:patrolend            TRUE                                                           
11 date_filter_type:patrolstart          TRUE                                                           
12 date_filter_type:RecordDate           FALSE                                                          
13 date_filter_type:waypointdate         TRUE                                                           
14 date_filter_type:waypointlastmodified TRUE                                                           

$`WCS Chile - Patrol Monitoring 1.0 [SMART]`$todo_patrullas
# A tibble: 14 × 2
   feature                               value                                                          
   <chr>                                 <chr>                                                          
 1 folder                                WCS Chile - Patrol Monitoring 1.0 [SMART]/Tests Diego (no usar)
 2 query_name                            todo_patrullas                                                 
 3 typeKey                               patrolobservation                                              
 4 uuid                                  de879d8d-3491-4418-9a7e-a72b6958372f                           
 5 spatial_query                         TRUE                                                           
 6 date_filter_type:assetdeploymentdate  FALSE                                                          
 7 date_filter_type:missionenddate       FALSE                                                          
 8 date_filter_type:missionstartdate     FALSE                                                          
 9 date_filter_type:missiontrackdate     FALSE                                                          
10 date_filter_type:patrolend            TRUE                                                           
11 date_filter_type:patrolstart          TRUE                                                           
12 date_filter_type:RecordDate           FALSE                                                          
13 date_filter_type:waypointdate         TRUE                                                           
14 date_filter_type:waypointlastmodified TRUE                               
```


So to load the data returned by the "todo_patrullas" query (spatial_query=TRUE);
therefore, I can request the output as shp

```
data_from_connect(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password",
name_conservation_area = "WCS Chile - Patrol Monitoring 1.0 [SMART]",
query_name = "todo_patrullas",
type_output = "shp",'                                  
date_filter="waypointlastmodified",
start_date="2020-01-01", #YYYY-MM-DD; date_filter_type:patrolstart is TRUE  
end_date="2023-06-01", #YYYY-MM-DD; date_filter_type:patrolend isTRUE
srid=4326,
UTM_zone=NULL)

out
```

# To create a species attribute:

Basically the function 'create_species_function' creates a Data Model that only 
has a species attribute as a tree with two levels. Then this species attribute-only
data model can be merged into the user's Data Model and the attribute becomes available. 
The species tree attribute can be flattened to a list in SMART Desktop. Check the
final step lo learn how to merge the species attribute data model created with this
function to the data model in SMART Desktop at the end of this section.

The species tree attribute has a key and a label. The key that the species attribute
will have is provided through the argument 'species_attribute_key' that cannot have
spaces, weird characters, or uppercase letters. The label is provided through the 
argument 'species_in_language'. 

The attribute 'language' is the abbreviation of the language in which you are using
the attribute and it has to consistent with the languages available in the data 
model in SMART Desktop. If your Conservation Area has as language "english" and
"laotian", then the argument 'language' can be either "en" or "lo" but not "fr" 
(french). To find the abbreviations check the options in your Conservation Area 
Data Model on the top-left corner and click on the downward arrow.

The first level of the species tree attribute is a taxonomy level (class, 
order, family, etc.). The options of these taxonomy levels (e.g., 'aves', 
'mammalia, etc.) also have keys and labels. The function has the 'first_tax_level'
attribute to provide the first level taxonomy with the same key and label. Provide
a character vector of the same length than species_key and species_label.

```
first_tax_level<-c(rep('Aves', 3), rep("Mammalia", 3))

species_key=c('Circus melanoleucos', 
              'Spilornis cheela', 
              'Dendrocygna javanica', 
              'Bos gaurus', 
              'Bos javanicus', 
              'Capricornis milneedwardsii maritimus')

species_label=c('Pied harrier',
                'Crested serpent eagle',
                'Lesser whistling duck',
                'Wild water buffalo',
                'Red muntjac',
                'Large-antlered Muntjac')
                
create_species_attribute(
           language="en", 
           species_attribute_key="species_smarter",
           species_in_language="species",
           first_tax_level=first_tax_level,
           species_key=species_key,
           species_label=species_label)                
```

The output of the function is an xml file named "species_smarter_attribute.xml"
saved in your working directory. To add the attribute to your data model, open 
the Conservation Area, open the Data Model, find the "Merge Data Model" button 
in the bottom left and select the "species_smarter_attribute.xml" file. Add the 
attribute to the Category or Categories (search for the attribute usig the key
provided in 'species_attribute_key'.
