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

#' do we want to look at dref and dstaar?
#' 
#' we DO need to clean up our column headers, at least in cstaar
#' removes first two characters (CD) and last three characters (18R), which are
#' present in _all_ column headers and therefore irrelevant for further 
#' analysis
names(cstaar) <- str_sub(names(cstaar), 3, -4) 
cstaar_01 <- cstaar %>% 
  rename(CAMPUS = M) 

#' rename based on the three variables of interest:
#'   subject
#'   demographics
#'   proficiency
cstaar_02 <- cstaar_01 %>% 
  select_all(~str_replace(., "B00", "blackaa")) %>% 
  select_all(~str_replace(., "A00", "all")) %>%
  select_all(~str_replace(., "I00", "amind")) %>%
  select_all(~str_replace(., "300", "asian")) %>%
  select_all(~str_replace(., "R00", "atrisk")) %>%
  select_all(~str_replace(., "L00", "ELL")) %>%
  select_all(~str_replace(., "E00", "lowses")) %>%
  select_all(~str_replace(., "F00", "female")) %>%
  select_all(~str_replace(., "H00", "latinx")) %>%
  select_all(~str_replace(., "M00", "male")) %>%
  select_all(~str_replace(., "400", "pacisl")) %>%
  select_all(~str_replace(., "S00", "sped")) %>%
  select_all(~str_replace(., "200", "twoplus")) %>%
  select_all(~str_replace(., "W00", "white")) %>%
  select_all(~str_replace(., "AM0", "_math_")) %>% 
  select_all(~str_replace(., "AR0", "_ela_")) %>% 
  select_all(~str_replace(., "AC0", "_science_")) %>% 
  select_all(~str_replace(., "AS0", "_soc_")) %>% 
  select_all(~str_replace(., "AW0", "_writing_")) %>% 
  select_all(~str_replace(., "12", "meets")) %>% 
  select_all(~str_replace(., "13", "masters")) %>% 
  select_all(~str_replace(., "1S", "approaches")) 

names(cstaar_02)

#' spread proficiency into its own column
