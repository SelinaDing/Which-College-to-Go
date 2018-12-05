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

# Deriving the options for the "state".

state_choices <- college %>% 
  group_by(state) %>% 
  summarise()

# Define the UI
# Use a pretty theme

ui <- fluidPage(theme = shinytheme("slate"),
                
                # Application title
                
                titlePanel("Which College to Go"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select Input for "State".
                    
                    selectInput(inputId = "state",
                                label = "State",
                                choices = state_choices,
                                selected = "NY"),
                    
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
  
  # Geojoining the states geolayer and the cleaned mappable data.
  
  data <- geo_join(states, college, "STUSPS", "state")
  
  # Creating a color palette based on the number range in the total column
  
  pal <- colorNumeric("BuPu", domain=data$mr_kq5_pq1)
  
  # popup the income of students
  
  popup_sb <- paste0(as.character(data$name), ": ", as.character(data$k_median))
  
  # render map
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
    addProviderTiles(provider = "CartoDB") %>%
    setView(-98.483330, 38.712046, zoom = 4) %>%
    addMarkers(lng = data$lon, lat =data$lat, popup = ~popup_sb) %>%
    addPolygons(data = data , 
                fillColor = ~pal(data$mr_kq5_pq1), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2) %>%
    addLegend(pal = pal, 
              values = data$mr_kq5_pq1, 
              position = "bottomright", 
              title = "Mobility Rate: Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 20% of the income distribution") %>%
    addSearchOSM() %>%
    addReverseSearchOSM()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)