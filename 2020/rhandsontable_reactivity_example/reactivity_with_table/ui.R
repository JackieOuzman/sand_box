
#bring in the library that I will be working with
library(shiny)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rhandsontable)



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
     column(width=6,rHandsontableOutput("cost"))
 ),#fluid row bracket 3
 fluidRow(
   column(width=6,verbatimTextOutput("cost_tb1")) #this is just a check
 ),#fluid row bracket 4
fluidRow(
  column(width=6,actionButton("saveBtn", "Save")) 
),#fluid row bracket 5
fluidRow(
  column(width=6,rHandsontableOutput("cost2")) 
),#fluid row bracket 6
fluidRow(
  column(width=6,plotOutput("plot1")) 
)#fluid row bracket 7

) #fluidPage bracket


   






