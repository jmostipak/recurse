#' this file is used to clean up an existing dataset as well as explore
#' various graphs to be used in the Shiny app I'll be using for the RC
#' Fellowship application


# set up environment ------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)
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

sy1617_plot <- sy1617_wrangle_02


# write data to file ------------------------------------------------------

write_csv(sy1617_plot, here("data", "wrangled_school_data.csv"))

# graphs for app ----------------------------------------------------------
#' data to use: sy1617_plot
#' model after this: https://shiny.rstudio.com/gallery/movie-explorer.html
movies %>%
  ggvis(x = xvar, y = yvar) %>%
  layer_points(size := 50, size.hover := 200,
               fillOpacity := 0.2, fillOpacity.hover := 0.5,
               stroke = ~has_oscar, key := ~ID) %>%
  add_tooltip(movie_tooltip, "hover") %>%
  add_axis("x", title = xvar_name) %>%
  add_axis("y", title = yvar_name) %>%
  add_legend("stroke", title = "Won Oscar", values = c("Yes", "No")) %>%
  scale_nominal("stroke", domain = c("Yes", "No"),
                range = c("orange", "#aaa")) %>%
  set_options(width = 500, height = 500)

