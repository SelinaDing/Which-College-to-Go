#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(ggmap)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Which College to Go"),
   leafletOutput("mymap"),
   p()
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # load data
  ma_c <- read_rds("ma_c.rds")
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(provider = "CartoDB") %>%
      setView(lat = 41.964569, lng = -70.596297, zoom = 7) %>%
      addMarkers(lng = ma_c$lon, lat = ma_c$lat, popup = ma_c$name) %>%
      addSearchOSM() %>%
      addReverseSearchOSM()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

