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
#' importing .csv data files
csv_import <- function(directory, file_name){
  read_csv(here(directory, file_name),
           guess_max = 1200)
}

#' recoding column headers
#' (could make this more efficiency by using a variation on Colin's regex 
#' builder: https://colinfay.me/purrr-text-wrangling/)
recode <- function(data){
  data %>% 
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
}

# staar_data_import -------------------------------------------------------
data_path <- "version_2.0/data/for_use"
cref <- csv_import(data_path, "CREF.csv") 
cstaar <- csv_import(data_path, "CSTAAR_ALL1.csv")
dref <- csv_import(data_path, "DREF.csv")
dstaar <- csv_import(data_path, "DSTAAR_ALL1.csv")


# wrangling cstaar data ----------------------------------------------------------
#' we DO need to clean up our column headers, at least in cstaar
#' removes first two characters (CD) and last three characters (18R), which are
#' present in _all_ column headers and therefore irrelevant for further 
#' analysis
names(cstaar) <- str_sub(names(cstaar), 3, -4) 
cstaar_01 <- cstaar %>% 
  rename(campus = M) %>% 
  recode()

#' gather column headers so that we can parse them later
cstaar_02 <- cstaar_01 %>% 
  gather(key = "col_headers", "rate", -campus) %>% 
  #' separate col_header row into discrete components
  separate(col = col_headers, into = c("category", "subject", "proficiency"),
           sep = "_") 


# joining cstaar and CREF data ------------------------------------------------------------
campus <- cref %>% 
  select(CAMPUS, DISTNAME:GRDTYPE) %>% 
  select_all(~str_to_lower(.)) %>% 
  left_join(cstaar_02, by = "campus") %>% 
  rename(campus_rate = rate)


# wrangling dstaar data ---------------------------------------------------
#' this data has the same inherent problems as the cstaar data
#' based on data dictionary, the same code can be used
names(dstaar) <- str_sub(names(dstaar), 3, -4) 
dstaar_01 <- dstaar %>% 
  rename(district = STR) %>% 
  recode()

#' gather column headers so that we can parse them later
dstaar_02 <- dstaar_01 %>% 
  gather(key = "col_headers", "rate", -district) %>% 
  #' separate col_header row into discrete components
  separate(col = col_headers, into = c("category", "subject", "proficiency"),
           sep = "_") 

# joining dstaar and DREF data ------------------------------------------------------------
district <- dref %>% 
  select(DISTRICT, D_RATING) %>% 
  select_all(~str_to_lower(.)) %>% 
  left_join(dstaar_02, by = "district") %>% 
  rename(dist_rate = rate)


# master file -------------------------------------------------------------
campdist <- campus %>% 
  left_join(district, by = c("district", "category", 
                             "subject", "proficiency")) %>% 
  mutate(campus_rate = as.numeric(campus_rate),
         dist_rate = as.numeric(dist_rate)) 

# write it out ------------------------------------------------------------
write_csv(here("version_2.0/data/for_use", "campdist.csv"))

# clean it up -------------------------------------------------------------
rm(campus, cref, 
   cstaar, cstaar_01, cstaar_02,
   csv_import, 
   data_path, district, dref,
   dstaar, dstaar_01, dstaar_02,
   recode)
