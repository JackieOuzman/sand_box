#devtools::install_github("AnalytixWare/ShinySky")


library(shiny)
library("gapminder")
library(dplyr)
library(ggplot2)
library(shinysky)



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Select outcome"),

inputPanel(uiOutput("data1"),   ## uiOutput - gets the UI from the server
           uiOutput("data2"),
           uiOutput("data3")),

mainPanel(hotable("hotable1")
))
##########################
# ui <- fluidPage(
#   titlePanel("test fluid"),
#   mainPanel(hotable("hotable1")
# 
#   )
# )


