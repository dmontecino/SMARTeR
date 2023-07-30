# SMARTeR

A set of functions to support your work with the Spatial Monitoring and 
Reporting Tools (SMART; https://smartconservationtools.org/) Conservation Areas,
including a preparing a species tree attribute from a csv and load it to a Data Model,
run a query from SMART Connect and load the output in your R session, and convert
a Configurable model into a flat table with the corresponding Categories, Attributes,
and options (for multilist, lists, and tree attributes).


The package uses the dplyr (>= 1.1.2), janitor (>= 2.2.0), jsonlite (>= 1.8.5),
lubridate (>= 1.9.2), magrittr (>= 2.0.3), purrr (>= 1.0.1), rvest (>= 1.0.3),
sf (>= 1.0.13), stringr (>= 1.5.0), tibble (>= 3.2.1), tidyr (>= 1.3.0), 
xml2 (>= 1.3.4), zoo (>= 1.8.12) packages. 

# To run a query in SMART Connect and load data in R:

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

I suggest to use these functions to patrol queries, observation queries, etc, 
but not summary queries. Summary queries will be loaded but the row names as seen
in SMART Desktop will be returned as an unnamed column. For this queries, only the
first column header is read as the column names, while secondary column header are
read as data. So, I suggest to create an observation query that returns all the 
data, load it in R, and start creating summaries, stats, etc here. Summary queries
are already summarized data so what's the point.


```
query_info(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password")

```

and I get something like this: 


```
                              
```





So to load the data returned by the "all_patrol_data" query:

```
data_from_connect(
server_url = "my_connect_server_url",
user = "my_user",
password = "my_password",
name_conservation_area = "WCS Chile - Patrol Monitoring 1.0 [SMART]",
query_name = "informacion_patrullas",
type_output = "shp",'                                  
date_filter="waypointlastmodified",
start_date="2020-01-01", #YYYY-MM-DD
end_date="2023-06-01", #YYYY-MM-DD
srid=4326,
UTM_zone=NULL)

out
```
To create a species attribute in any language from a csv, first prepare the csv. 
You can create any csv you want for I suggest to follow the template 



