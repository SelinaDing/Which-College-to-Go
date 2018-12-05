#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

# load packages

library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(ggmap)
library(tidyverse)
library(tigris)

# load data

college <- read_rds("college.rds")

# Getting states geo files.

states <- states(cb=T)

# Deriving the options for the "college".

college_choices <- college %>% 
  group_by(name) %>% 
  summarise()

# Define the UI
# Use a pretty theme

ui <- fluidPage(theme = shinytheme("slate"),
                
                # Application title
                
                titlePanel("Which College to Go"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select Input for "State".
                    
                    selectInput(inputId = "college",
                                label = "Name of the College",
                                choices = college_choices,
                                selected = "Harvard University"),
                    
                    # And a button allowing users to download my data and further poke around 
                    
                    downloadButton(outputId = "download_data", 
                                   label = "Download data")),
                  
                  # Define the main panel
                  
                  mainPanel(
                    
                    # Use a tab layout to separate the various elements
                    
                    tabsetPanel(type = "tabs",
                                
                                # Summary for tabs.
                                
                                tabPanel("About this map", htmlOutput("about")),
                                
                                # map output
                                
                                tabPanel("Map for Opportunity", leafletOutput("mymap")),
                                
                                # Tab for viewing plot
                                
                                tabPanel("Plot", plotOutput("plot")),
                                
                                # Tab for the data view
                                
                                tabPanel("Data Table", dataTableOutput("data")))
                    
                  )))

# Define server

server <- function(input, output) {
  
  # render map
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles(provider = "CartoDB") %>%
    setView(-98.483330, 38.712046, zoom = 4) %>%
    addMarkers(lng = college$lon, lat = college$lat, popup = as.character(college$k_median)) %>%
    addSearchOSM() %>%
    addReverseSearchOSM()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)