# SMARTeR
A set of functions to load query data from SMART Connect to the R environment. 

These functions use the rvest package to start a SMART Connect session, find the queries available per Conservation Area in your SMART Connect instance, select the query output you want to load in R, in a comma-separated format (.csv) or as a sf object (.shp). 

The function "data.from.connect" loads data from the conservation area provided in the argument "name.conservation.area" as requested by the query provided in the "query_name" argument. The formats available are "csv" and "shp" and they are selected in the "type" argument (type = "shp" or "csv"). 

For now, the functions woork for quey types: "PatrolQuery" and "PatrolObservationQuery", although if you contact me, it "should" be reallyeasy to add other query types. 

To see the functions script go to the R folder.

To load the functions: 

source("https://raw.github.com/")
