#' this file is used to clean up an existing dataset as well as explore
#' various graphs to be used in the Shiny app I'll be using for the RC
#' Fellowship application


# set up environment ------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggvis)
library(here)
library(skimr)

# import data -------------------------------------------------------------
sy1617_import <- read_csv(here("data", "final_data_1617.csv"))


# initial wrangling -------------------------------------------------------
names(sy1617_import)

#' what's in GROUP?
sy1617_import %>% 
  group_by(GROUP) %>% 
  summarise(n = n())  # campus, district, state grouping variables

#' choosing variables of possible interest for app
sy1617_wrangle <- sy1617_import %>% 
  rename(group = GROUP) %>% 
  select(group, district_name, campus_name, year, grade_band:percent)

#' missing data!
skim(sy1617_wrangle)

sy1617_wrangle %>% 
  filter(is.na(denom)) %>% 
  group_by(group) %>% 
  summarise(n = n())

sy1617_wrangle %>% 
  filter(is.na(denom)) %>% 
  View()

#' removing missing numeric data
sy1617_wrangle_02 <- sy1617_wrangle %>% 
  filter(!is.na(denom),
         !is.na(num))

skim(sy1617_wrangle_02)

#' confirming that missing data at this point is tied to state and 
#' district data, and not individual campuses
sy1617_wrangle_02 %>% 
  filter(is.na(campus_name)) %>% 
  group_by(group) %>% 
  summarise(n = n())

sy1617_wrangle_02 %>% 
  filter(is.na(grade_band)) %>% 
  group_by(group) %>% 
  summarise(n = n())

# write data to file ------------------------------------------------------
math <- sy1617_wrangle_02 %>% 
  filter(subject == "math") %>% 
  select(-subject, -num, -denom, -percent) %>% 
  rename(math_rate = rate)

sy1617_plot <- sy1617_wrangle_02 %>% 
  filter(subject == "reading") %>% 
  select(-subject, -num, -denom, -percent) %>% 
  rename(reading_rate = rate) %>% 
  left_join(math, by = c("group", "district_name", "campus_name",
                         "year", "grade_band", "grade", 
                         "proficiency"))

write_csv(sy1617_plot, here("data", "wrangled_school_data.csv"))

# graphs for app ----------------------------------------------------------
#' data to use: sy1617_plot
#' model after this: https://shiny.rstudio.com/gallery/movie-explorer.html


sy1617_plot %>% 
  filter(year == 17,
         reading_rate > 0 & reading_rate < 100,
         math_rate > 0 & math_rate < 100,
         district_name == "DALLAS ISD") %>% 
  ggvis(~math_rate, ~reading_rate, fill = ~proficiency) %>% 
  layer_points(size := 50, size.hover := 200,
             fillOpacity := 0.2, fillOpacity.hover := 0.5,
             stroke = ~proficiency)

# # code from example app - uses ggvis
# movies %>%
#   ggvis(x = xvar, y = yvar) %>%
#   layer_points(size := 50, size.hover := 200,
#                fillOpacity := 0.2, fillOpacity.hover := 0.5,
#                stroke = ~has_oscar, key := ~ID) %>%
#   add_tooltip(movie_tooltip, "hover") %>%
#   add_axis("x", title = xvar_name) %>%
#   add_axis("y", title = yvar_name) %>%
#   add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
#   scale_nominal("stroke", domain = c("Yes", "No"),
#                 range = c("orange", "#aaa")) %>%
#   set_options(width = 500, height = 500)


# clean it up! ------------------------------------------------------------

rm(initial_import, 
   sy1617_import, 
   sy1617_wrangle, sy1617_wrangle_02,
   math)
