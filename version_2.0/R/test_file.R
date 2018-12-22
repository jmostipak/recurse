#' objectives:
#'   split code into relevant files
#'   incorporate `source` if using multiple packages
#'     >> requires local loading of `here` package
#'   improve/modularize existing code
#'   create initial EDA exploration (see JK resources)


# environment set-up ------------------------------------------------------

library(here)
source(here("version_2.0/R", "00_packages.R"))

# staar_data_import -------------------------------------------------------
cref <- read_csv(here("version_2.0/data/for_use", "CREF.csv"))
skim(cref)
