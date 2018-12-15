#' sample Shiny app for RC application
#' goal is to take a non-reactive plot (below) and during the pair programming 
#' activity add reactivity via either Input widgets or R2D3

library(shiny)
library(ggvis)
library(here)

data <- read_csv(here("data", "wrangled_school_data.csv"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     data %>% 
       filter(year == 17,
              reading_rate > 0 & reading_rate < 100,
              math_rate > 0 & math_rate < 100) %>% 
       ggvis(~math_rate, ~reading_rate, fill = ~proficiency) %>% 
       layer_points(size := 50, size.hover := 200,
                    fillOpacity := 0.2, fillOpacity.hover := 0.5,
                    stroke = ~proficiency)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

