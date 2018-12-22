#' objectives:
#'   split code into relevant files
#'   incorporate `source` if using multiple packages
#'     >> requires local loading of `here` package
#'   improve/modularize existing code
#'   create initial EDA exploration (see JK resources)


# environment set-up ------------------------------------------------------
library(here)
source(here("version_2.0/R", "00_packages.R"))

# functions ---------------------------------------------------------------
csv_import <- function(directory, file_name){
  read_csv(here(directory, file_name),
           guess_max = 1200)
}

# staar_data_import -------------------------------------------------------
data_path <- "version_2.0/data/for_use"
cdref <- csv_import(data_path, "CREF.csv") 
cstaar <- csv_import(data_path, "CSTAAR_ALL1.csv")
dref <- csv_import(data_path, "DREF.csv")
dstaar <- csv_import(data_path, "DSTAAR_ALL1.csv")
names(cref)
names(dref)
