#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(tidyverse)

# Define the UI
# Use a pretty theme

ui <- fluidPage(fluidPage(theme = shinytheme("united")),
                
                # Application title
                
                titlePanel("Which College to Go"),
                
                sidebarLayout(
                  sidebarPanel(
                    textInput(inputId = "name",
                              label = "Search a college name:"),
                    
                    # And a button allowing users to download my data and further poke around 
                    
                    downloadButton(outputId = "download_data", 
                                   label = "Download data")),
                  
                  # Define the main panel
                  
                  mainPanel(
                    
                    # Use a tab layout to separate the various elements
                    
                    tabsetPanel(type = "tabs",
                                tabPanel("About this map", htmlOutput("about")),
                                tabPanel("Map for Opportunity", leafletOutput("mymap")))
                    
                  )))

# Define server

server <- function(input, output) {
  # load data
  college <- read_rds("college.rds")
  # render map
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles(provider = "CartoDB") %>%
    addMarkers(lng = college$lon, lat = college$lat, popup = college$name) %>%
    addSearchOSM() %>%
    addReverseSearchOSM()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

