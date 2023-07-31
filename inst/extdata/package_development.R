require(available)
require(devtools)
require(here)
library(R2oxygen)
library(dang)



devtools::dev_sitrep()

available("SMARTeR")

create_package("SMARTeR")

use_r('create_species_attribute')

usethis::use_github_links()

usethis::use_mit_license()


usethis::edit_r_environ()

document()

usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("rvest", min_version = TRUE)
usethis::use_package("jsonlite", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("magrittr", min_version = TRUE)
usethis::use_package("xml2", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("janitor", min_version = TRUE)
usethis::use_package("sf", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("utils", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)

usethis::use_pipe()

load_all() # 

dang::checkPackageAsciiCode(dir = ".")
stringi::stri_escape_unicode("’")

check()

exists("SMARTeR", where = globalenv(), inherits = FALSE) # ahould be false

?query_info

server_url = "https://karukinkaconnect.smartconservationtools.org/server"
user= "dmontecino"
password = "Pancha176"
SMARTeR::query_info(server_url = server_url, user = user, password = password)

out<-flat_conf_model(path_conf_model="/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/SMART_FOR_HEALTH_2023/Rangers/Configurable_Model_July_2023_Obs_Spec.xml",
                     language_interest="en")

out<-query_info(server_url = "https://wcshealth.smartconservationtools.org/server",
                user = "smart123",  
                password = "smart123")



out<-data_from_connect(server_url = "https://karukinkaconnect.smartconservationtools.org/server",
                       user = "dmontecino",
                       password = password,
                       name_conservation_area = "WCS Chile - Patrol Monitoring 1.0 [SMART]",
                       query_name = "numero_patrullajes_por_individuo_por_mes",
                       type_output = "csv",
                       date_filter="waypointlastmodified",
                       start_date="2020-01-01", #YYYY-MM-DD
                       end_date="2023-06-01", #YYYY-MM-DD
                       srid=4326,
                       UTM_zone=NULL)

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

SMARTeR::create_species_attribute(
           language="en", 
           species_attribute_key="species_smarter",
           species_in_language="species",
           first_tax_level=first_tax_level,
           species_key=species_key,
           species_label=species_label)
    

SMARTeR::flat_conf_model(path_conf_model = "/Users/DMontecino/OneDrive - Wildlife Conservation Society/SMART/SMART_WILDLIFE_HEALTH/SMART_FOR_HEALTH_2023/Rangers/Configurable_Model_July_2023_Obs_Spec.xml")
