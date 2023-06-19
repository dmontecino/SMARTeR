date_filters_types_available<-function(){

date_filters_types_available = list(
  
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

 date_filters_types_available<-
   date_filters_types_available[order(names(date_filters_types_available))] 
 
 return(date_filters_types_available)
 }
 
 
date_filters_types_available()
