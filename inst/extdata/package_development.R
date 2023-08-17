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

install()
