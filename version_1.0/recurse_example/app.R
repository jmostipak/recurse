#' sample Shiny app for RC application
#' goal is to take a non-reactive plot (below) and during the pair programming 
#' activity add reactivity via either Input widgets or R2D3

library(shiny)
library(readr)
library(dplyr)
library(ggvis)
library(here)
library(magrittr)

data <- read_csv(here("data", "wrangled_school_data.csv"))

# defining the UI for app
ui <- fluidPage(
  fluidRow(
    
    # app title
    column(12, offset = 1, titlePanel("Campus Math and Reading rates for 2017")),
    
    #' filter widget
    selectInput(inputId = "proficiency", label = "profs are meets or approaches",
                choices = c("meets", "approaches")
    ),
    
    hr(),
    fluidRow(column(3, verbatimTextOutput("value"))),
    
    column(5, offset = 1,
  
           # app sidebar for slider input
           sidebarLayout(
             sidebarPanel("Input widgets for reactivity will go here"
             ),
             #' possible filters: 
             #' meets/approaches proficiencies
             #' subject
             #' three districts 

             
    column(6,
           # plot of sample data
           mainPanel(
             ggvisOutput("plot1")
             )
           )         
    )
    )
  )
)

# Define server logic required to draw plot
server <- function(input, output) {
  
  proficiency <- reactive({
    
    # m <- data %>% 
    #   filter(proficiency >= proficiency) %>% 
    #   arrange(math_rate, reading_rate)
    
    if (data$proficiency != "meets") {
      proficiency <- paste0("%", data$proficiency, "%")
      m <- data %>% filter(proficiency %in% proficiency)
    }  
  })
  

  
  #' plot
  
   vis <- reactive({
     data %>% 
       filter(year == 17,
              reading_rate > 0 & reading_rate < 100,
              math_rate > 0 & math_rate < 100,
              district_name %in% c("DALLAS ISD", "FORT WORTH ISD", 
                                   "UPLIFT EDUCATION")) %>% 
       ggvis(~math_rate, ~reading_rate, fill = ~proficiency) %>% 
       layer_points(size := 50, size.hover := 200,
                    fillOpacity := 0.2, fillOpacity.hover := 0.5,
                    stroke = ~proficiency)
   })
   
   vis %>% bind_shiny("plot1")
}

# run app
shinyApp(ui = ui, server = server)

