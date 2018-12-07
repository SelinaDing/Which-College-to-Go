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
library(stargazer)
library(tidyverse)
library(tigris)
library(scales)

# load the rds data written previously

college <- read_rds("college.rds")

# change the format of variables

college <- college %>%
  mutate(k_median = dollar(k_median)) %>%
  mutate(par_median = dollar(par_median)) %>%
  mutate(mr_ktop1_pq1 = percent(mr_ktop1_pq1/100)) %>%
  mutate(par_q1 = percent(par_q1/100)) %>%
  mutate(par_top1pc = percent(par_top1pc/100)) %>%
  mutate(mr_kq5_pq1 = percent(mr_kq5_pq1/100)) %>%
  mutate(trend_parq1 = percent(trend_parq1/100)) 

# Deriving the options for the "college".

college_choices <- college %>% 
  group_by(name) %>% 
  summarise()

# Define variable choices for user

variable_choices <- c("Child Income" = "k_median",
                  "Parent Income" = "par_median",
                  "Parents in the Bottom 20%" = "par_q1", 
                  "Parents in the Top 1%" = "par_top1pc",
                  "Mobility Rate" = "mr_kq5_pq1",
                  "American Dream" = "mr_ktop1_pq1",
                  "Change in % of Bottom 20% Parents" = "trend_parq1")

# Define the UI
# Use a pretty theme

ui <- fluidPage(theme = shinytheme("slate"),
                
                # Application title
                
                titlePanel("Which College to Go"),
                
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select Input for "college"
                    
                    selectInput(inputId = "college",
                                label = "Name of the College: ",
                                choices = college_choices,
                                selected = "Harvard University"),
                    
                    # Select Input for "Variable"
                    
                    selectInput(inputId = "variable",
                                label = "Variable: ",
                                choices = variable_choices,
                                selected = "American Dream"),
                    
                    # And a button allowing users to download my data and further poke around 
                    
                    downloadButton(outputId = "download_data", 
                                   label = "Download data"),
                    
                    # variable explanations for people not familiar with the topic
                    
                    htmlOutput("description")),
                    
                  
                  # Define the main panel
                  
                  mainPanel(
                    
                    # Use a tab layout to separate the various elements
                    
                    tabsetPanel(type = "tabs",
                                
                                # Summary for tabs.
                                
                                tabPanel("About this map", 
                                         htmlOutput("about"),
                                         
                                         # embed a picture
                                         
                                         HTML('<center><img src="thefadingame.jpg" height = 500 width = 550 ></center>')
                                         ),
                                
                                # map output
                                
                                tabPanel("Map for Opportunity", leafletOutput("mymap")),
                                
                                
                                # Tab for the data view
                                
                                tabPanel("Data Table", dataTableOutput("data")))
                    
                  )))

# Define server

server <- function(input, output) {
  
  # Variable Description 
  
  output$description <- renderText({
    "<br/><b>Variable Description</b><br/>
    <br/>1.<b>Child Income: </b><br/>Median child individual earnings in 2014
    <br/>2.<b>Parents Income: </b><br/>Median parent household income
    <br/>3.<b>Parents in the Bottom 20%: </b><br/>Fraction of parents in the Bottom 20% of the income distribution
    <br/>4.<b>Parents in the Top 1%: </b><br/>Fraction of parents in the Top 1% of the income distribution
    <br/>5.<b>Mobility Rate: </b><br/>Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 20% of the income distribution
    <br/>6.<b>American Dream: </b><br/>(Upper-Tail Mobility Rate) Percent of students who have parents in the Bottom 20% of the income distribution and reach the Top 1% of the income distribution
    <br/>7.<b>Change in % of Bottom 20% Parents: </b><br/>Change in % of Parents from the Bottom 20% of the income distribution between the 1980 and 1991 cohorts)
"
  })
  
  # render readme
  
  output$about <- renderText({
    "<br/><b>Which college in the US offers you the best chance to achieve the American Dream?</b><br/><br/>
    This map is a map of opportunity. 
    It answers the question using data for over 30 million college students from 1999-2013.
    The map offers you the insight of the median income and intergenerational mobility at each college in the United States.<br/><br/> 
    "
  })
  
  # render map
  
  output$mymap <- renderLeaflet({
    
    # filter data to input college
    
    data <- college %>%
      filter(name == input$college) %>%
      select(name, lon, lat, input$variable)
    
    # setview of input college
    
    leaflet() %>% 
    addProviderTiles(provider = "CartoDB") %>%
    setView(data$lon, data$lat, zoom = 13) %>%
    
    # label markers as college name, and popup the variable user choose when clicking
    
    addCircleMarkers(lng = college$lon, lat = college$lat, 
                     label = college$name,
                     popup = paste0(data[4]), 
                     radius = 10) %>%
    
    # users can also search a certain address or region
      
    addSearchOSM() %>%
    addReverseSearchOSM()
   })
  
  # table of variable users choose
  
  output$data <- renderDataTable({
    college %>%
      rename("Name of the College" = name,
             "State" = state) %>%
      
      # I can't change the colname of input$variable
      
      select("Name of the College", State, input$variable)
  })
    
  }

# Run the application 
shinyApp(ui = ui, server = server)