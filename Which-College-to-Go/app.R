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
library(scales)

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
  
  # render readme
  
  output$about <- renderText({
    "Which college in the United States offers you the best chance to achieve the American Dream?
    This map is a map of opportunity. 
    It answers this question using data for over 30 million college students from 1999-2013.
    The map offers you insight of the median income and intergenerational mobility at each college in the United States. 
    "
  })
  
  # render map
  
  output$mymap <- renderLeaflet({
    
    # filter data to input college
    
    college <- college %>%
      mutate(k_median = dollar(k_median)) %>%
      mutate(mr_ktop1_pq1 = percent(mr_ktop1_pq1/100))
    
    data <- college %>%
      filter(name == input$college)
    
    # setview of input college
    
    leaflet() %>% 
    addProviderTiles(provider = "CartoDB") %>%
    setView(data$lon, data$lat, zoom = 13) %>%
    addCircleMarkers(lng = college$lon, lat = college$lat, 
                     popup = paste0("<b>", college$name, "</b>","<br/>", "Income Median: ",as.character(college$k_median),
                                    "</b>","<br/>", "Mobility Rate: ", as.character(college$mr_ktop1_pq1)), 
                     radius = 3) %>%
    addSearchOSM() %>%
    addReverseSearchOSM()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)