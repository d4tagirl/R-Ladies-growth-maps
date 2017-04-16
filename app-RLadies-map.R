library(shiny)
library(dplyr)
library(lubridate)
library(stringr)
library(ggmap)
library(purrr)
library(tidyr)

load("/Users/Daniela/rladies_growing/RLadies_growing.Rdata")

library("maps")
library("ggthemes")

mp <- ggplot(world.cities, package = "maps") +
  ggplot2::borders("world", colour = "gray80", fill = "gray80") +
  theme_map()

library("plotly")

ui <- fluidPage(
   # 
   # Application title
   titlePanel("R-Ladies Growth"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "created_at", 
                    label = "Date created:",  
                    min =   as.Date("2016-04-20") , 
                    max =   as.Date("2017-04-20") ,
                    value = as.Date("2017-04-20"),
                    animate = animationOptions(playButton = icon('play', "fa-1x"),
                                               pauseButton = icon('pause', "fa-1x")))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("map")#,
#         verbatimTextOutput("event")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_df <- reactive({
    users_2 %>% 
      filter(created_at < input$created_at) 
  })
  
   output$map <- renderPlotly({
     
     ggplotly(mp +
       geom_point(aes(x = lon, y = lat, text = paste("city: ", location), size = followers, label = created), 
                  data = selected_df(), colour = 'purple', alpha = .5) +
       scale_size_continuous(range = c(1, 10)), 
       tooltip = c("text", "size", "label"))
   })
   
   # output$event <- renderPrint({
   #   d <- event_data("plotly_hover")
   #   if (is.null(d)) "Hover on a point!" else d
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

