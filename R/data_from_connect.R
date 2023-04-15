library(rvest)
library(tidyverse)
library(jsonlite)
library(janitor)
library(sf)

# --------------------------------------------------------------------------------------------------------- #
# function to save the data as a temp, unzip it, and open it as a sf object when the type selected is "shp" #
# --------------------------------------------------------------------------------------------------------- #

    dlshape=function(shploc, shpfile) { 
      temp=tempfile()  
      # download.file(shploc, temp)
      writeBin(shploc, temp)
      unzip(temp) 
      fp <- sf::read_sf(shpfile) 
      return(fp)} 
