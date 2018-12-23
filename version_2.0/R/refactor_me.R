# refactoring notes -------------------------------------------------------
#'  split into multiple files 
#'    > utilize `source` for packages and functions
#'    > import and wrangle
#'    > EDA
#'  
#'  SETUP
#'    > modify package list prn
#'    
#'  IMPORT
#'    > add guess_max to csv_import function
#'    > assign path to data_path variable
#'    > refactor import to join and select upstream from wrangle
#'      -- regex for columns of interest
#'    
#'  WRANGLE
#'    > rename column headers 
#'    > add campus_rate and district_rate signifiers before join
#'  
#'  EDA
#'    > see Cox article, JK resources
#'  


# environment setup -------------------------------------------------------
# packages used for analysis and visualization
library(tidyverse) 
library(here)   # file paths
library(ggridges)  # ridgeplots


# functions ---------------------------------------------------------------

csv_import <- function(directory, file_name){
  read_csv(here(directory, file_name))
}

# import ------------------------------------------------------------------
#' NEW SPRING-ONLY preliminary data file for 2018, created by CM from 
#' scripts in outdated_scriptsfile within this repo (impact_report_FY2018)

# prelim_18 <- read_csv(here("results", "prelim_tame_tidy.csv"), #OLD (Spring+June) FILE NAME
prelim_18 <- read_csv(here("results", "spring_prelim_tame_tidy.csv"), #NEW (Spring) FILE NAME
                      #' grade needs to be explicitly coerced to character to 
                      #' accommodate `eoc` value
                      col_types = cols(grade = col_character())) %>% 
  # adding in the data source for subsequent `rbind` with CM's files
  mutate(data_source = "prelim") 

#' files from CM work in Final_vs_Prelim repo
pre_final_17 <- csv_import("results", "master_prelim_vs_tapr_2017.csv") 
pre_final_1516 <- csv_import("results", "tapr_only_1516.csv") 
pre_final_15_to_17 <- pre_final_1516 %>% 
  mutate(data_source = "tapr") %>% 
  select(district_name, campus_id, campus, year, GRDTYPE, 
         demographics, subject, proficiency, tt_school, program,
         program_subtype, data_source, numerator, total_tested) %>% 
  rbind(pre_final_17)

# wrangling files to facilitate rbind -------------------------------------

#str(prelim_18)
#str(pre_final_15_to_17)

#' prelim_18 uses "mathematics" and "reading", while pre_final_15_to_17 uses
#' "math" and "reading"
#' 
#' I also need to drop the "did not meet" and "masters" proficiencies from 
#' the preliminary data 
prelim_18_01 <- prelim_18 %>% 
  filter(proficiency %in% c("meets", "approaches"),
         demographics %in% c("American Indian or Alaskan Native",
                             "Asian", "Black or African American", 
                             "Economically Disadvantaged", "Hispanic/Latino",
                             "Native Hawaiian or Other Pacific Islander", 
                             "Two or More Races", "White", "whole_campus")) %>% 
  mutate(subject = if_else(subject == "mathematics", "math", subject),
         demographics = if_else(demographics == "American Indian or Alaskan Native", 
                                "American Indian", demographics), 
         demographics = if_else(demographics == "Native Hawaiian or Other Pacific Islander",
                                "Pacific Islander", demographics))

#' need to fix campus_id = NA in the preliminary data file because OF COURSE
campus_non_na <- prelim_18_01 %>% 
  filter(!is.na(campus_id))

campus_na <- prelim_18_01 %>% 
  filter(is.na(campus_id)) 

campus_na_01 <- campus_na %>% 
  separate(campus, c("campus", "campus_id"), sep = "[:punct:]", extra = "drop") %>% 
  select(district_name, campus_id, campus, everything())

prelim_18_02 <- campus_na_01 %>% 
  rbind(campus_non_na)

#' prelim_18_01 uses NA when a school is not enrolled or affiliated with a
#' Teaching Trust program, while pre_final_15_to_17 uses a lowercase x
#' this could be problematic - where else is there a lowercase 'x'?
prelim_final_01 <- pre_final_15_to_17 %>% 
  na_if("x")

#' in addition, we don't need preliminary data for prelim_final_01, as the
#' only preliminary data we're using is for the 2018 school year
prelim_final_02 <- prelim_final_01 %>% 
  filter(data_source == "tapr")

#' the prelim_18 file has individual grade levels, while the 
#' prelim_final_15_to_17 file has GRDTYPE - this is because TAPR data for meets
#' does not release individual grade level data, but only grade band data. we
#' will not need grade band data, therefore will need to roll-up individual
#' grade and grade band data to reflect the whole school for a given subject,
#' proficiency, and demographic split

prelim_18_03 <- prelim_18_02 %>% 
  group_by(district_name, campus_id, campus, tt_school,
           demographics, year, proficiency, subject,
           program, program_subtype, data_source) %>% 
  summarise(campus_num = sum(numerator, na.rm = TRUE),
            campus_total_tested = sum(total_tested, na.rm = TRUE))  %>% 
  ungroup()


prelim_final_03 <- prelim_final_02 %>% 
  group_by(district_name, campus_id, campus, tt_school,
           demographics, year, proficiency, subject,
           program, program_subtype, data_source) %>% 
  #' this will combine schools that are listed two times or more,
  #' based on being categorized as more than one grade band (GRDTYPE)
  summarise(campus_num = sum(numerator),
            campus_total_tested = sum(total_tested)) %>% 
  ungroup()


# combining datasets ------------------------------------------------------

pf_1518 <- prelim_18_03 %>% 
  rbind(prelim_final_03)


# By-Hand QA: school name standardization ----------------------------------

#' fixing school name inconsistencies 
#' the inconsistencies are a result of longer names in prelim than final
#' in the following code I will only be fixing TT schools (for now...)

#' Prelim allows longer school-name lengths than the TAPR does. 
#' These three are length changes:

pf_1518_01 <- pf_1518 
pf_1518_01$campus[pf_1518_01$campus==
                    "UPLIFT EDUCATION - INFINITY PREPAR"]<- "UPLIFT EDUCATION - INFINITY PREPARATORY PRI"
pf_1518_01$campus[pf_1518_01$campus==
                    "UPLIFT EDUCATION - UPLIFT GRAND PR"]<- "UPLIFT EDUCATION - UPLIFT GRAND PREPARATORY" 
pf_1518_01$campus[pf_1518_01$campus==
                    "THOMAS A EDISON MIDDLE LEARNING CE"] <- "THOMAS A EDISON MIDDLE LEARNING CENTER"

# These three are name changes:

pf_1518_01$campus[pf_1518_01$campus==
                    "WILLIAM L CABELL EL"] <- "CHAPEL HILL PREPARATORY"
pf_1518_01$campus[pf_1518_01$campus==
                    "ALBERT SIDNEY JOHNSTON EL"] <- "CEDAR CREST EL"
pf_1518_01$campus[pf_1518_01$campus==
                    "ONESIMO HERNANDEZ EL"] <- "MONTESSORI ACADEMY AT ONESIMO HERNANDEZ"
pf_1518_01$campus[pf_1518_01$campus==
                    "ANN RICHARDS MIDDLE"] <- "ANN RICHARDS STEAM ACADEMY"


#' now relabeling TT schools whose names changed in 2018

pf_1518_01$tt_school[pf_1518_01$campus== "CHAPEL HILL PREPARATORY"] <- "TRUE"
pf_1518_01$campus_id[pf_1518_01$campus== "CHAPEL HILL PREPARATORY"] <- "57905119"
pf_1518_01$tt_school[pf_1518_01$campus== "CEDAR CREST EL"] <- "TRUE"
pf_1518_01$campus_id[pf_1518_01$campus== "CEDAR CREST EL"] <- "57905163"
pf_1518_01$tt_school[pf_1518_01$campus== "MONTESSORI ACADEMY AT ONESIMO HERNANDEZ"] <- "TRUE"
pf_1518_01$campus_id[pf_1518_01$campus== "MONTESSORI ACADEMY AT ONESIMO HERNANDEZ"] <- "57905269"
pf_1518_01$tt_school[pf_1518_01$campus== "ANN RICHARDS STEAM ACADEMY"] <- "TRUE"
pf_1518_01$campus_id[pf_1518_01$campus== "ANN RICHARDS STEAM ACADEMY"] <- "57905353"

#'fixing the schools that had zeros added to campus id in 2018
#' first Lakewood El
pf_1518_01$campus_id[pf_1518_01$campus_id== "057905171"] <- "57905171"
pf_1518_01$tt_school[pf_1518_01$campus_id== "57905171"] <- "TRUE"
pf_1518_01$program[pf_1518_01$campus_id== "57905171"] <- "ALP"
pf_1518_01$program_subtype[pf_1518_01$campus_id== "57905171"] <- "C4"
pf_1518_01$campus[pf_1518_01$campus_id== "57905171"] <- "LAKEWOOD EL"
#' next Ben Milam
pf_1518_01$campus_id[pf_1518_01$campus_id== "057905184"] <- "57905184"
pf_1518_01$tt_school[pf_1518_01$campus_id== "57905184"] <- "TRUE"
pf_1518_01$program[pf_1518_01$campus_id== "57905184"] <- "TEAMS"
pf_1518_01$program_subtype[pf_1518_01$campus_id== "57905184"] <- "TEAMS 1"
pf_1518_01$campus[pf_1518_01$campus_id== "57905184"] <- "BEN MILAM EL"
#' next San Jacinto
pf_1518_01$campus_id[pf_1518_01$campus_id== "057905207"] <- "57905207"
pf_1518_01$tt_school[pf_1518_01$campus_id== "57905207"] <- "TRUE"
pf_1518_01$program[pf_1518_01$campus_id== "57905207"] <- "TEAMS"
pf_1518_01$program_subtype[pf_1518_01$campus_id== "57905207"] <- "TEAMS 2"
pf_1518_01$campus[pf_1518_01$campus_id== "57905207"] <- "SAN JACINTO EL"
#' lastly, Pleasant Grove
pf_1518_01$campus_id[pf_1518_01$campus_id== "057905273"] <- "57905273"
pf_1518_01$tt_school[pf_1518_01$campus_id== "57905273"] <- "TRUE"
pf_1518_01$program[pf_1518_01$campus_id== "57905273"] <- "TEAMS"
pf_1518_01$program_subtype[pf_1518_01$campus_id== "57905273"] <- "TEAMS 1"
pf_1518_01$campus[pf_1518_01$campus_id== "57905273"] <- "PLEASANT GROVE EL"

# End of By-Hand QA -------------------------------------------------------




# adding “tt-led” indicator -----------------------------------------------
tt_led_01 <- read_csv(here("data", "tt_leader_list_FY2019.csv"))
#' as is, this table is largely useless since it doesn't follow any of the
#' standard naming conventions used in the preliminary or TAPR data files.
#' however, because the list is relatively short, I can manually create a
#' match using the %in% functionality


pf_1518_02 <- pf_1518_01 %>% 
  mutate(tt_led = if_else(campus_id %in% 
                            c(57905110, 57905304, 57905129, 57905148,
                              57905149, 57905204, 57905210, 57905220,
                              57905125, 57905278, 57905028, 57905145,
                              57905101, 57905171, 57905054, 57905059,
                              57803109), 
                          TRUE, FALSE))

#' #' confirming that all data for 2018 is preliminary, while 2015 - 2017 data is 
#' #' final(TAPR) data - works!
#' pf_1518_01 %>% 
#'   group_by(year, data_source) %>% 
#'   summarise(n = n())  
#' 
#' #' why are there 10,540 prelim observations for 2018, and significantly fewer
#' #' observations for each of the previous years? 
#' pf_1518_01 %>% 
#'   distinct(year, campus, tt_school) %>% 
#'   group_by(year, tt_school) %>% 
#'   summarise(n = n())  # this shows a consistent number of schools each year
#' 
#' pf_1518_01 %>% 
#'   group_by(year, demographics) %>% 
#'   summarise(n = n()) %>% 
#'   View()

# calculating growth metrics ----------------------------------------------
#' ultimately we know that Teaching Trust schools are doing better in growth
#' rather than absolute comparisons to either the state or the district.
#' as such, we'll need to calculate year-over-year growth values. based on
#' CM's work, I feel confident that we can use the preliminary 2018 data as
#' a proxy for TAPR scores for the 2017-2018 school year, however we will
#' need to clearly indicate that this is what we're doing, and that we're 
#' likely under-estimating final scores by 5 - 11 percentage points.
#' 
#' this code calculates year over year growth metrics:
pf_1518_03 <- pf_1518_02 %>% 
  mutate(perc_prof = 100 * (campus_num / campus_total_tested)) %>% 
  group_by(campus, demographics, proficiency, subject) %>%  
  arrange(year) %>% 
  mutate(yoy = perc_prof - lag(perc_prof)) %>% 
  ungroup()


#' this code gets rid of any "total_tested" equalling zero
pf_1518_04 <- pf_1518_03 %>%
  filter(campus_total_tested!=0)


# writing master .csv to file ---------------------------------------------

write_csv(pf_1518_04, here("results", "spring_master_prelim_final_15_to_18.csv"))

# clean it up -------------------------------------------------------------
rm(pre_final_1516, pre_final_17, pre_final_15_to_17,
   prelim_18, prelim_18_01, prelim_18_02, prelim_18_03,
   prelim_final_01, prelim_final_02, prelim_final_03,
   campus_na, campus_na_01, campus_non_na,
   tt_led_01,
   pf_1518, pf_1518_01, pf_1518_02)