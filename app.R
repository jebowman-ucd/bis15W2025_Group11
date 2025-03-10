#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(sf)
library("shinythemes")
library(dplyr)
library("tidyr")

bigfoot <- read.csv("data/bigfoot_updated_CAonly_noNA.csv")

temp_shapefile <- tempfile()
download.file("https://www2.census.gov/geo/tiger/TIGER2019/COUSUB/tl_2019_06_cousub.zip", temp_shapefile)
unzip(temp_shapefile)

sf_county <- read_sf('tl_2019_06_cousub.shp')


ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("My Bigfoot Sightings App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("x", 
                  "Select Classification", 
                  choices = unique(bigfoot$classification), 
                  selected = "Class A"),
      selectInput("y", 
                  "Select Weather", 
                  choices = unique(bigfoot$conditions), 
                  selected = "Clear")
      
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
   
)


server <- function(input, output) {

  output$map <- renderLeaflet({
    
    filtered_data <- bigfoot %>% 
      dplyr::filter(classification == input$x) %>%
      dplyr::filter(conditions == input$y)
    
    
    filtered_data %>% 
      leaflet(sf_county) %>% 
      addTiles() %>%  # Optionally keep or remove this line if not needed
      addCircleMarkers(
        lng = ~longitude, 
        lat = ~latitude, 
        popup = ~as.character(bigfoot$date), 
        label = ~as.character(bigfoot$date),
        radius = 5,           
        color = "blue",       
        fillColor = "cyan",   
        fillOpacity = 0.7     
      ) %>%
      setView(lng = -121.771598, 
              lat = 38.533867, 
              zoom = 5.5) %>%
      addPolygons(data=sf_county, color = "black", weight = 1, fillColor="thistle", opacity=0.7 )%>%
      addProviderTiles('Stamen.Terrain')
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
