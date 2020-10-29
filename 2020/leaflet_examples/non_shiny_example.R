
library(shiny)
library(leaflet)
# points <- eventReactive(input$recalc, {
#   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
# }, ignoreNULL = FALSE)

points <- cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)



library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map
