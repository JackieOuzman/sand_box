#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)
#library(shinyWidgets)
#library(readxl)
library(dplyr)
library(lubridate)

#this is not using the shiny dashbaord - but the layout is better
#how to get this fluid page layout with dashboard??

source('utils.R')

ui <- fluidPage(
  titlePanel("Tell me more about your farm"),
  sidebarLayout(
    sidebarPanel(
    hr(),
  
    selectInput("stationID", 
              label = h4("Where is your farm?"),
              choices = c("Waikerie" = "Waikerie",
                          "Carwarp" = "Carwarp",
                          "Ouyen" = "Ouyen", 
                          "Karoonda" = "Karoonda",
                          "Murlong" = "Murlong", 
                          "Yenda" = "Yenda", 
                          "Lameroo" ="Lameroo", 
                          "Bute" ="Bute", 
                          "Brimpton Lake" = "Brimpton Lake", 
                          "Cadgee" = "Cadgee"), 
              selected = "Waikerie"),
    
    checkboxGroupInput(
      "mangement_options",
      label = h3("What are you considering?"),
      choices = list("Wetting agents" = "wetting_agent", "Sowing on edge of row" = "sow_edge", 
                     "Spading with no inputs" = "spade_no_input", 
                     "Spading with shallow inputs" = "spade_shallow_input",
                     "Ripping with no inputs" = "rip_no_input", 
                     "Ripping with shallow inputs" = "rip_shallow_input",
                     "Ripping with deep inputs" = "rip_deep_input"),
                      selected = "wetting_agent"),
    
    checkboxGroupInput(
      "depth",
      label = h3("Depth of ripping?"),
      choices = list("Ripping to 30 cm" = "to30","Ripping to 50 cm" = "to50",
                     "Ripping to 60 cm" = "to60","Ripping to 70 cm" = "to70"),
                      selected = "to30"),
    
    h3("Crop sequence for 10 years"),
    
    h6("Code for crops is:"),
    #br(),
    h6("wh =wheat, ba =barley, ca= canola, leg = grain legume , pas =pasture"),
    selectizeInput("crop_seq_zone1", 
                   label="Crop type:",
                   choices =list(
                  "Year1"= c("Yr1_wh","Yr1_ba","Yr1_can","Yr1_leg","Yr1_pas"),
                  "Year2"= c("Yr2_wh","Yr2_ba","Yr2_can","Yr2_leg","Yr2_pas"),
                  "Year3"= c("Yr3_wh","Yr3_ba","Yr3_can","Yr3_leg","Yr3_pas"),
                  "Year4"= c("Yr4_wh","Yr4_ba","Yr4_can","Yr4_leg","Yr4_pas"),
                  "Year5"= c("Yr5_wh","Yr5_ba","Yr5_can","Yr5_leg","Yr5_pas"),
                  "Year6"= c("Yr6_wh","Yr6_ba","Yr6_can","Yr6_leg","Yr6_pas"),
                  "Year7"= c("Yr7_wh","Yr7_ba","Yr7_can","Yr7_leg","Yr7_pas"),
                  "Year8"= c("Yr8_wh","Yr8_ba","Yr8_can","Yr8_leg","Yr8_pas"),
                  "Year9"= c("Yr9_wh","Yr9_ba","Yr9_can","Yr9_leg","Yr9_pas"),
                  "Year101"= c("Yr10_wh","Yr10_ba","Yr10_can","Yr10_leg","Yr10_pas")
                  ),
                  options = list(maxItems = 10),
                  multiple = TRUE
                  ),
    #actionButton("add_crop", "add"),
    #actionButton("reset", "clear"),
    textOutput("table_crop")
    ),
    
    
  
    mainPanel(
      textOutput("site"), #check on site
      tableOutput("table1"), #site
      tableOutput("table2"), #list of treatmnets
      tableOutput("table3"),  #ripping depth
      tableOutput("test"),
      tableOutput("deciles_table"),
      tableOutput("xxx"),
      textOutput("what_is")
      
      
      )
    ))
 


