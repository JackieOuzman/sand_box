
#bring in the library that I will be working with
library("devtools")
library(shiny)
library("gapminder")
library(dplyr)
library(ggplot2)
library(readxl)
library(shinysky)
#library(AnalytixWare)
library(tidyverse)
library(shinysky)



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h1("Name of app")),

fluidRow(
  column(width=4, uiOutput("data1")),   ## uiOutput - gets the UI from the server
  column(width=4, uiOutput("data2")),
  column(width=4, uiOutput("data3"))
),#fluid row bracket 1
## Cost table
fluidRow(
  column(width=6,h2("Cost of modification Scenario 1"))
),#fluid row bracket 2
  fluidRow(
    column(width=6,hotable("hotable1")),
       ),#fluid row bracket 3
fluidRow(
  column(width=6,verbatimTextOutput("mod1")),
  column(width=6,verbatimTextOutput("cost_tb1"))
)#fluid row bracket 4


) #fluidPage bracket

   






