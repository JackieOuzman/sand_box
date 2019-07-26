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
library(lubridate)

source('utils_outline_idea.R')

#empty dashboard
header <-   dashboardHeader()

#this is the list of all tabs in the app
sidebar <-  dashboardSidebar(
            sidebarMenu(
              menuItem("The farm and ripping", tabName = "tell_me", icon = icon("question")),
              menuItem("Wheat", tabName = "set_up_farm", icon = icon("th")),
              menuItem("Ripping cost", tabName = "cost_ripping", icon = icon("list")),
              menuItem("Commodities & production costs", tabName = "extra", icon = icon("dollar")),
              menuItem("Results", tabName = "results", icon = icon("bar-chart-o"))
              #,menuItem("test", tabName = "test")
              #need to change tab names to something more meaninful              
#icon(name, class = NULL, lib = "font-awesome")            
#fa-cloud-sun", lib = "font-awesome") 


             
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
                                  choices = c("Waikerie" =24018, 
                                              "Carwarp" = 76005,
                                              "Ouyen" = 76047, 
                                              "Karoonda" = 25006, 
                                              "Murlong" = 18046, 
                                              "Yenda" = 75079, 
                                              "Lameroo" = 25509, 
                                              "Bute" = 21012, 
                                              "Brimpton Lake" = 18005, 
                                              "Cadgee" = 26099), 
                                  selected = "Waikerie"),
                      numericInput("production_area", #need to change this to something more meaninful
                                   label = "Area of production (ha)",
                                   value = 400,
                                   min = 10,
                                   max= 10000,
                                   step = 0.5),
                      
                      textOutput("name_of_met"),
                      
                      tableOutput("metfile")#,
                      
                      
      ), 
      
      
      column(6,
             wellPanel(
               
               checkboxGroupInput(
                 "mangement_options",
                 label = h3("What are you considering?"),
                 choices = list("Shallow ripping with inputs" = "rip_shallow_input", #need to change this to something more meaninful
                                "Deep ripping with inputs" = "rip_deep_input"),  #need to change this to something more meaninful
                 selected = "rip_shallow_input")
              
             ) #well pannel
      ) #column bracket
      
      )#this is fluid Row bracket
    ), #tabItem  
    
    
#The second tab
    tabItem(
      tabName = "set_up_farm",
      
        
            wellPanel(
            h4("Wheat crop"),
           
                      
           
            br(),
            
            
           
           numericInput("P5", #need to change this to something more meaninful
                        label = "P5 for wheat yield (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("P50", #need to change this to something more meaninful
                        label = "P50 for wheat yield  (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("P90", #need to change this to something more meaninful
                        label = "P90 for wheat yield  (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5)
           
           
          
      ), #well pannel bracket
      valueBoxOutput("yld_pot_wheat")
      ),
    
    
    
    
    
    

#the thrid tab


  ##### costs of ripping ######

#temp tab
  tabItem(
    tabName = "cost_ripping" , #need to change this to something more meaninful
    tabBox(


                
  #########          Shallow ripping         #########              
  ######### first one ripping with  inputs ####


                tabPanel(h4("shallow ripping"),
          wellPanel(
          numericInput("costs_ripping", 
                     label = h4("Cost for ripping with shallow inputs $/ha"), #need to change this to something more meaninful
                      value = 70, 
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
                                     'year 5' = "5"),
                     selected = 1,
                     multiple = TRUE)
          ) #well pannel
          
),# tab pannel 


#########          deep ripping         #########              
######### first one ripping with inputs ####
                
               
          tabPanel(h4("deep ripping"), 
      wellPanel(
                numericInput("rip_deep_cost", 
                 label = h4("Cost for ripping with deep inputs $/ha"),
                                value = 90, 
                                min = 0,
                                    max = 2000,
                                    step = 10),
                   
                  selectizeInput("rip_deep_year",
                                      label = h4("Ripping applied in which year?"),
                                      choices = list('before analysis'= "0",
                                                     'year 1' = "1",
                                                     'year 2' = "2",
                                                     'year 3' = "3",
                                                     'year 4' = "4",
                                                     'year 5' = "5"),
                                      selected = 1,
                                      multiple = TRUE)
                   
                   )) #bracket for well pannel and tab box
        )), #bracket for tab item





#the fourth tab   
        tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
       
        
        
        
        box(width=8,
        title = "Results over 5 years",
        textOutput("name_of_results"),
        textOutput("check_df"),
        plotOutput("plot")
        ))#box2 bracket
        )), #fluid bracket
        #), #tabItem bracket







     
    
    
    
    
#the fifth tab
     tabItem(
        tabName = "extra",
        h3("Price for wheat"),
        numericInput("a",
                     label = "Farm gate price wheat ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        h3("Nitrogen for average year - needs work"),
        numericInput("N_applied",
                     label = "Nitrogen applied (Kg/t)",
                     value = 200,
                     min = 0,
                     max= 400,
                     step = 5),
        numericInput("cost_N",
                     label = "Cost of nitrogen ($/t)",
                     value = 560,
                     min = 0,
                     max= 1000,
                     step = 10),
        h3("Freight"),
        numericInput("freight",
                     label = "Freight per tonne",
                     value = 17,
                     min = 0,
                     max= 100,
                     step = 1),
        h3("Insurance and levies"),
        numericInput("insurance",
                     label = "Insurance as % of revenue",
                     value = 1.1,
                     min = 0.1,
                     max= 2.0,
                     step = 0.1),
        numericInput("levies",
                     label = "Levies as % of revenue",
                     value = 1.1,
                     min = 0.1,
                     max= 2.0,
                     step = 0.1),
        h3("Variable Costs"),
        numericInput("variable_cost",
                     label = "Variable cost",
                     value = 185,
                     min = 0,
                     max= 300,
                     step = 1)
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
  
  ####MET FILE WORK ####
  
  met <- reactive({
    function_met(input$stationID) #this is the first function
  })
  
  water_aval<- reactive({
    function_water_aval(met()) 
  })
  decile<- reactive({
    function_decile(water_aval()) 
  })
  
  decile5_yld_pot_wheat <- reactive({
    function_decile5_yld_pot_wheat(water_aval())
  })
  
  decile5_yld_pot_pulses <- reactive({
    function_decile5_yld_pot_pulses(water_aval())
  })
  
  
  #####################################################################################  
  ###########                    CREATING DF FOR FARM                ##################
  #####################################################################################  
  base_df1 <- reactive({
   map_df(input$mangement_options,
           function_base_df1)  
           #,discount = input$discount)
  })
  
  #current yields  
  making_df_current <- reactive({
    function_making_df_current(input$P5, input$P50, input$P90, input$a, base_df1())
  })
  
  

  
  #####################################################################################   
  ################               CREATING DF FOR TREATMENTS                    ########
  #####################################################################################  
  
  
  #create a new df for treatments crop, yr, costs etc
  rip_shallow_df <- reactive({
    function_rip_shallow_input_df(making_df_current(), input$year_for_ripping, input$costs_ripping)
  })
  
  
  #create a new df for treatments crop, yr, costs etc
  rip_deep_df <- reactive({
    function_rip_deep_input_df(making_df_current(), input$rip_deep_year, input$rip_deep_cost)
  })
  
  treatment_bind <- reactive({
    function_treatment_bind(rip_shallow_df(), rip_deep_df())
  })
  
  ####################################################################################   
  ################               Join Treatment df to the farm df            ########
  #####################################################################################  

  final_treatment_farm <- reactive({
    function_final_treatment_farm(making_df_current(), treatment_bind())
  })
  
  
  ####################################################################################   
  ################      create gross margins from final_treatment_farm        ########
  #####################################################################################  
 
  #economic indicators
  economic_indicators <- reactive({
    function_economic_indicators(final_treatment_farm(), input$production_area, 
                                 input$N_applied, input$cost_N, input$insurance,
                                 input$levies, input$freight, input$variable_cost)
  })
  
  plot <- reactive({
    function_plot(economic_indicators() )
  })
  
  

  ####### group of render OUTPUTS ########
  
  #this is new
  output$metfile <- renderTable({
    decile() 
  })
 
  
  output$yld_pot_wheat <- renderValueBox({
    valueBox(decile5_yld_pot_wheat(), "Yield potential of wheat  t/ha")
  })

  
  ####################################################################################
  #############             I was using this as a check #############################
  ####################################################################################
    
# output$name_of_results <- renderText({
#        paste0("check: ",economic_indicators())
#      })
  ####################################################################################

  
  
 

 output$plot = renderPlot({
   plot()
 })
 
}
shinyApp(ui, server)

