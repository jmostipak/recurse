#' sample Shiny app for RC application
#' goal is to take a non-reactive plot (below) and during the pair programming 
#' activity add reactivity via either Input widgets or R2D3

library(shiny)
library(readr)
library(dplyr)
library(ggvis)
library(here)

data <- read_csv(here("data", "wrangled_school_data.csv"))

# defining the UI for app
ui <- fluidPage(
   
   # app title
   titlePanel("DISD Campus Math and Reading rates for 2017"),
   
   # app sidebar for slider input
   sidebarLayout(
      sidebarPanel("Input widgets for reactivity will go here"
      ),
      
      # plot of sample data
      mainPanel(
        ggvisOutput("plot1")
      )
   )
)

# Define server logic required to draw plot
server <- function(input, output) {
   
   vis <- reactive({
     data %>% 
       filter(year == 17,
              reading_rate > 0 & reading_rate < 100,
              math_rate > 0 & math_rate < 100,
              district_name == "DALLAS ISD") %>% 
       ggvis(~math_rate, ~reading_rate, fill = ~proficiency) %>% 
       layer_points(size := 50, size.hover := 200,
                    fillOpacity := 0.2, fillOpacity.hover := 0.5,
                    stroke = ~proficiency)
   })
   
   vis %>% bind_shiny("plot1")
}

# run app
shinyApp(ui = ui, server = server)

