library(tibble)
library(purrr)
library(dplyr)
library(devtools)


date_filter_types_available_per_query_type = list(
  
  'surveymissiontrack' = c('missiontrackdate', 'missionstartdate', 'missionenddate'),
  'patrolquery' = c('patrolstart', 'patrolend', 'waypointlastmodified'), 
  'surveywaypoint' = c('waypointdate', 'missionstartdate', 'missionenddate','waypointlastmodified'),
  'observationgrid'= c('waypointdate','waypointlastmodified'),
  'entitygrid' = c('waypointdate'),
  'surveyobservation' =  c('waypointdate', 'missionstartdate', 'missionenddate', 'waypointlastmodified'),
  'assetobservation' = c('waypointdate', 'waypointlastmodified'),
  'observationobservation'= c('waypointdate', 'waypointlastmodified'), 
  'i2_obs_query'= 'waypointdate',
  'surveymission'= c('missionstartdate', 'missionenddate'),
  'observationwaypoint'= c('waypointdate', 'waypointlastmodified'),
  'entitysummary'= 'waypointdate',
  'assetwaypoint'= c('waypointdate','waypointlastmodified'),
  'entitywaypoint'= 'waypointdate',
  'entityobservation'= 'waypointdate',
  'assetdeploymentsummary'= 'assetdeploymentdate',
  'i2_record_query'= 'RecordDate',
  'assetsummary'= c('waypointdate','waypointlastmodified'),
  'patrolwaypoint'= c('waypointdate','patrolstart','patrolend','waypointlastmodified'),
  'patrolobservation'= c('waypointdate','patrolstart','patrolend','waypointlastmodified'),
  'patrolsummary'= c('waypointdate','patrolstart','patrolend','waypointlastmodified'),
  'observationsummary'= c('waypointdate', 'waypointlastmodified'),
  'i2_record_summ_query'='RecordDate',
  'patrolgrid'= c('waypointdate','patrolstart','patrolend','waypointlastmodified'),
  'surveygrid'= c('waypointdate','missionstartdate','missionenddate','waypointlastmodified'),
  'surveysummary'=  c('waypointdate', 'missionstartdate', 'missionenddate','waypointlastmodified'))



all_date_filter_options<-
  sort(
    unique(
      unlist(
        date_filter_types_available_per_query_type, use.names = F)))

date_filter_types_available_per_query_type<-
  as.data.frame(
    do.call(rbind, 
            map(date_filter_types_available_per_query_type, 
                function(x) all_date_filter_options %in% x)))


colnames(date_filter_types_available_per_query_type)<-
  paste0("date_filter_type", ":", all_date_filter_options)


date_filter_types_available_per_query_type <- 
  date_filter_types_available_per_query_type %>%
  rownames_to_column(var = "typeKey")

#use_data_raw() #creates the data-raw/ folder and lists it in .Rbuildignore. 


#usethis::use_data(date_filter_types_available_per_query_type)

