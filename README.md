# SMARTeR

A set of functions to prepare data models and configurable models for the Spatial Monitoring and Reporting Tools (SMART; https://smartconservationtools.org/) Conservation Areas, retrieve data from data and configurable models (e.g., keys, categories, attribute names, and options per list or multilist attribute), and load the output of queries of specific Conservation Areas that are stored in SMART Connect. The output of the queries can be loaded as spatial data or csv depending on the query properties. The package also contains a function to identify the queries available in SMART Connect per Conservation Area, the properties of each specific query, and per query folder. This information is necessary to properly identify the output options available for each query.

The package uses the rvest, tibble, jsonlite, janitor, sf, dplyr, plyr, and purrr packages. 

More soon...

To load the functions in R copy and paste: 

source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/queries_available_per_conservation_area.R")

source("https://raw.githubusercontent.com/dmontecino/SMARTeR/main/R/data_from_connect.R")

First, call the function "query_info". This function has as arguments "server_url", "user", and "password". Provide the connect url in the server_url argument as character (e.g, "https://wcshealth.smartconservationtools.org/server") then your username and password as character in the corresponding arguments. This function will return a list. Each list element provides the conservation areas you have access to that have one query at least. Each element list has is named based on the corresponding conservation areas. Within each list element there is a vector with the queries available for the corresponding conservation area. The list element names and query names within are used in the function to load data as arguments (see next paragraph). 

Secondly, to load data returned by a query, use the function "data_from_connect". This function also has as arguments "server_url", "user", and "password", plus "name_conservation_area", "query_name", and "type". For the "name_conservation_area argument" provide the name of the conservation area holding the query of interest. The "name_conservation_area" is the name of the conservation area as returned by the output of the "queries_available_per_conservation_area" function (the corresponding list element name). The "query_name" is the name of the query as provided in the corresponding element of the list returned by "queries_available_per_conservation_area". The "type" options are "shp" or "csv". If the selection is csv, a tibble is created. If the selection is shp, a .zip file is saved as a temporary file, unzipped, and read as an sf object.

As an example, let'say I want to load all the data provided by the query "all_patrol_data" that belongs to the conservation area named "conservation area A". 

First, I run the "queries_available_per_conservation_area" function 

```
queries_available_per_conservation_area(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password")

```

and I get something like this: 


```
$`conservation area A [SMART]`

 [1] "all_patrol_data"                                           "other_query_1"                                             
 [3] "other_query_4"                                             "other_query_3"                                                            

$`conservation area B [SMART]`

 [1] "query_1"                                                   "query_2"                              
```





So to load the data returned by the "all_patrol_data" query:

```
out<- data_from_connect(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password",
name_conservation_area = "conservation area A [SMART]",
query_name = "all_patrol_data",
type = "csv")

out
```

The documentation will be improved soon and documentation for the remaining functions will become available soon.

This project could become a package at some point if more functions are added.
