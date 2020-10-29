library(shiny)
library(leaflet)
library(dplyr)

my_data <- read.csv("Q:/Sandy Soils Project files_CSIRO/GIS/CSP00203_SiteLocation_coordinates_2020.csv")
names(my_data)
str(my_data)




library(leaflet)
library(shiny)
shinyApp(
  ui = fluidPage(
    # sliderInput(inputId = "time", 
    #             label = "Years Before Present:", 
    #             min = -50, max = 15000, value = 0, step = 500),
    # tags$div(title = "This input has a tool tip",
    #          selectInput(inputId = "taxon", 
    #                      label = "Taxon of Interest", 
    #                      choices = sort(unique(pollen_subset$Taxon)))),
    leafletOutput("MapPlot1")
  ),
  
  server = function(input, output) {
    
    output$MapPlot1 <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%   # Add default OpenStreetMap map tiles
        setView(lng = 140.78, lat = -34.50, zoom = 6)
      
    })
    
    observe({
      
      # age <- input$time
      # taxon <- input$taxon
      # 
      # sites <- pollen_subset %>% 
      #   filter(findInterval(pollen_subset$Age, c(age - 250, age + 250)) == 1 &
      #            pollen_subset$Taxon %in% taxon)
      # 
      leafletProxy("MapPlot1") %>% clearMarkers() %>% 
        addCircleMarkers(lng = my_data$longitude,
                         lat = my_data$latitude,
                         opacity = my_data$Pct)
    })
  },
  options = list(height = 600)
)

