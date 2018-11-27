#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(tidyverse)

source('utils_outline_idea.R')

#empty dashboard
header <-   dashboardHeader()

#this is the list of all tabs in the app
sidebar <-  dashboardSidebar(
            sidebarMenu(
              menuItem("Tell me more",
              tabName = "tell_me"),
              menuItem("Set up farm",
              tabName = "set_up_farm"),
              menuItem("Costs",
              tabName = "costs"),
              menuItem("Extra modifications",
              tabName = "extra"),
              menuItem("Results",
              tabName = "results")
              
              
  )#sidebarMenu
)#dshboradSider


body <- dashboardBody(
        
  tabItems(
    
    
    
#the first tab    
    
    tabItem(
      tabName = "tell_me",
      
      fluidRow(column(6,
      selectInput("stationID", 
                  label = h3("Where is your farm?"),
                  choices = c("Waikerie", "Carwarp","Ouyen", "Karoonda", "Murlong", "Yenda", "Lameroo", "Bute", "Brimpton Lake", "Cadgee"), 
                  selected = "Waikerie"),
      
      numericInput(
        "total_size_farm",
        label = h3("Total size of yor farm ha"),
        value = 2500,
        min = 100,
        max = 6000,
        step =100)
      ), 
      
      
      
      #radioButtons(
      #  "numb_zones",
      #  label = h3("How many zones?"),
      #  choices = list("1 zone" = 1, "2 zones" = 2, "3 zones" = 3),
      #  selected = 1),
      
      column(6,
      wellPanel(
      
        checkboxGroupInput(
        "mangement_options",
        label = h3("What are you considering?"),
        choices = list("Wetting agents" = "wetter", 
                       #"Sowing on edge of row" = "sow_edge", 
                       #"Spading with no inputs" = "spade_no_inputs", 
                       #"Spading with shallow organic inputs" = "spade_organic",
                       #"Spading with shallow fertiliser as inputs" = "spade_fert",
                       "Ripping with no inputs" = "rip_no_inputs",
                       "Ripping with shallow organic inputs"= "rip_shallow_organic",
                       "Ripping with shallow fertiliser as inputs" = "rip_shallow_fert",
                       "Ripping with deep organic inputs"= "rip_deep_organic",
                       "Ripping with deep fertiliser as inputs" = "rip_deep_fert"), 
        selected = "rip_no_inputs"),
        radioButtons(
        "depth",
        label = h4("Depth of ripping?"),
        choices = list("Ripping to 30 cm" = "30",
                       "Ripping to 50 cm" = "50",
                       "Ripping to 60 cm" = "60",
                       "Ripping to 70 cm" = "70"),
        selected ="30")
      ) #well pannel
      ) #column bracket
      
      )#this is fluid Row bracket
    ), #tabItem  
    
    
    
#The second tab
    tabItem(
      tabName = "set_up_farm",
      
        
        tabsetPanel(
          tabPanel(h3("Zone1")), 
          
          
          
          #numericInput(
          #  "size_zone1",
          #  label = h4("Size of zone 1 (ha)"),
          #  value = 2500, #this needs to be total area of farm /3
          #  min = 2500,
          #  max = 6000, #can't be larger than total farm area
          #  step =100),
          
           
          
          wellPanel(
            h4("Crop sequence for 10 years"),
            h6("Code for crops is:"),
            #br(),
            h6("wh =wheat, ba =barley, ca= canola, leg = grain legume , pas =pasture"),
            br(),
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
                           selected = c("Yr1_wh", "Yr2_wh", "Yr3_wh" ,"Yr4_wh" ,"Yr5_wh", "Yr6_wh", "Yr7_wh", "Yr8_wh" ,"Yr9_wh", "Yr10_wh"),
                           options = list(maxItems = 10),
                           multiple = TRUE
            ),
           
            textOutput("text_size_farm"), # I want to move this up so its above select                   
           
            br(),
            
            
            fluidRow(column(6,
            
           numericInput("aa",
                        label = "Current wheat (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("bb",
                        label = "Current barley (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("cc",
                        label = "Current canola (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("dd",
                        label = "Current legume (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5)
            ), #column bracket
           
           column(6,
           
           numericInput("aaa",
                        label = "Potential wheat (t/ha)",
                        value = 4,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("bbb",
                        label = "Potential barley (t/ha)",
                        value = 4,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("ccc",
                        label = "Potential canola (t/ha)",
                        value = 4,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("ddd",
                        label = "Potential legume (t/ha)",
                        value = 4,
                        min = 0,
                        max= 8,
                        step = 0.5)
           ) #column bracket
            ) #fluidRow bracket
          
          #Add these (zone2 and zone3) in later when I work out the details
          #they will look like zone 1
          #tabPanel(h3("Zone2")),
          
          #tabPanel(h3("Zone3"))
          ) 
      )),
    
    
    
    
    
    

#the thrid tab


tabItem(
  tabName = "costs",
  tabsetPanel(
    tabPanel(h3("Ripping with no inputs")), 
  
  fluidRow(column(6,  
  h3("Upfront costs:"),
  h6("(Annual cost $/ha)"),
  numericInput("costs_ripping", 
             label = h4("Cost for ripping"),
              value = 80, 
              min = 0,
              max = 2000,
              step = 10),
  
  selectizeInput("year_for_ripping", 
             label = h4("Ripping applied in which year?"),
             choices = list('before analysis'= "0",
                             'year 1' = "1",
                             'year 2' = "2",
                             'year 3' = "3",
                             'year 4' = "4",
                             'year 5' = "5",
                             'year 6' = "6",
                             'year 7' = "7",
                             'year 8' = "8",
                             'year 9' = "9",
                             'year 10' = "10"),
             multiple = TRUE),
 
  h6("Note we suggets assigning ripping cost at year 0"),
  h6("Note costs for wetting agents applied and ... applied every year")
  ), #column bracket
  
  column(6,
  h3("Inseason costs:"),
  h6("(Average annual change in costs $/ha)"),
  numericInput("inseason_cost_ripping", 
               label = h4("Average annual change in costs for ripping"),
               value = 80, 
               min = 0,
               max = 2000,
               step = 10),
  
  h6("Note: inseason costs are likey to change with some treatment,"),
  h6("for example we fertiliser, seeding rates and other costs may change if ripping with inputs was implmented"),
  h6("this 'Average annual change in costs $/ha)' is an overall estimate how of variable costs will change.")
  ) #column bracket
  ) #fluidRow bracket
)), #this is the tabItem bracket



#the fourth tab   
        tabItem(
        tabName = "results",
        fluidRow(
          box(
        selectInput("Results for", 
              label = h3("Over how many years?"),
              choices = c("5 years" = "5yrs", "10 years" ="10yrs"), 
              selected = "5 years"),
        checkboxGroupInput("analysis", 
                    label = h3("What analysis?"),
                    choices = c("Undiscounted annual cash flow" = "cashflow_no_disc",
                                "discounted annual net cash flow" ="cashflow_disc_ann",
                                "Cummulative discounted cash flow" ="cashflow_cum_disc",
                                "Cummulative ROI not discounted" ="ROI_cum_no_disc",
                                "Cummulative ROI discounted" ="ROI_cum_disc",
                                "Benefit:Cost Ratio (discounted)" ="benefit_cost_ratio_disc",
                                "Net Present Value" ="NPV",
                                "Modified Internal Rate Return" ="MIRR"),
                                selected = "cashflow_disc_ann") #checkboxInput bracket
          ), #box1 bracket
        box(
        tableOutput("df_progress")
        )#box2 bracket
        ) #fluid bracket
        ), #tabItem bracket



     
    
    
    
    
#the fifth tab
     tabItem(
        tabName = "extra",
        numericInput("discount",
                     label = "Discount factor",
                     value = 0.6,
                     min = 0,
                     max= 1,
                     step = 0.02),
        numericInput("a",
                     label = "Farm gate price wheat ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        numericInput("b",
                     label = "Farm gate price barley ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        numericInput("c",
                     label = "Farm gate price canola ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        numericInput("d",
                     label = "Farm gate price legume ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10)
         )




#brackets for the tabItems dashboard    
)
)

          



#Display of the Ui



ui <- dashboardPage(header =dashboardHeader(), 
                    sidebar = sidebar, 
                    body = body
                    )


server <- function(input, output) {
  #group of reactive functions
  base_df1 <- reactive({
    function_base_df1 (input$crop_seq_zone1, input$discount)
  })
  
  fix_crop_name <- reactive({
    function_fix_crop_name (base_df1())
  })
  #current yields 
  making_df_current <- reactive({
    function_making_df_current(input$aa, input$bb, input$cc, input$dd)
  })
  flip_df_current <- reactive({
    function_flip_df_current(making_df_current())
  })
  join_current_df <- reactive({
    function_join_current_df(fix_crop_name(),flip_df_current())
  })
  #potential yield 
  making_df_potential <- reactive({
    function_making_df_potential(input$aaa, input$bbb, input$ccc, input$ddd)
  })
  flip_df_potential <- reactive({
    function_flip_df_potential(making_df_potential())
  })
  join_potential_df <- reactive({
    function_join_potential_df(join_current_df(),flip_df_potential())
  })
  
  #price 
  making_df_price <- reactive({
    function_making_df_price(input$a, input$b, input$c, input$d)
  })
  flip_df_price <- reactive({
    function_flip_df_price(making_df_price())
  })
  join_price_df <- reactive({
    function_join_price_df(join_potential_df(),flip_df_price())
  })
  
  
  
  #group of render outputs

 output$text_size_farm = renderText({
  paste("Total size of your farm is set to:", input$total_size_farm,"ha")
})

 output$df_progress = renderTable({
   join_price_df()
 })

}
shinyApp(ui, server)

