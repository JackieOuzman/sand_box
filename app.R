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
              menuItem("Ripping cost", tabName = "mitigation", icon = icon("list")),
              menuItem("Commodity prices", tabName = "extra", icon = icon("dollar")),
              menuItem("Results", tabName = "results", icon = icon("bar-chart-o"))
              #,menuItem("test", tabName = "test")
              
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
                      #actionButton("refreash",
                      #             label = "not working download"),
                      #actionButton("click",
                      #             label = "update wheat/barley"),
                      #actionButton("click2",
                      #             label = "update pulses"),
                      #numericInput(
                      #  "total_size_farm",
                      #  label = h3("Total size of yor farm ha"),
                      #  value = 2500,
                      #  min = 100,
                      #  max = 6000,
                      #  step =100),
                      
                      textOutput("name_of_met"),
                      
                      tableOutput("metfile")#,
                      #valueBoxOutput("yld_pot_wheat"),
                      #valueBoxOutput("yld_pot_pulses")
                      
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
                 choices = list("Shallow ripping with inputs" = "rip_shallow_fert",
                                "Deep ripping with inputs" = "rip_deep_fert"), 
                 selected = "rip_shallow_fert")
              
             ) #well pannel
      ) #column bracket
      
      )#this is fluid Row bracket
    ), #tabItem  
    
    
#The second tab
    tabItem(
      tabName = "set_up_farm",
      
        
            wellPanel(
            h4("Wheat crop"),
           
            #textOutput("text_size_farm"), # I want to move this up so its above select                   
           
            br(),
            
            
           
           numericInput("aa",
                        label = "P5 for wheat yield (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("bb",
                        label = "P50 for wheat yield  (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5),
           numericInput("cc",
                        label = "P90 for wheat yield  (t/ha)",
                        value = 3,
                        min = 0,
                        max= 8,
                        step = 0.5)
           
           
          
      ), #well pannel bracket
      valueBoxOutput("yld_pot_wheat")
      ),
    
    
    
    
    
    

#the thrid tab


  #####TRY MY mitigation costs ######

#temp tab
  tabItem(
    tabName = "mitigation" ,
    tabBox(


                
  #########          Shallow ripping         #########              
  ######### first one ripping with no inputs ####


                tabPanel(h4("shallow ripping"),
          wellPanel(
          numericInput("costs_ripping", 
                     label = h4("Cost for ripping with shallow inputs"),
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
######### first one ripping with organic inputs ####
                
               
          tabPanel(h4("deep ripping"), 
      wellPanel(
                numericInput("rip_deep_organic_cost", 
                 label = h4("Cost for ripping with deep inputs"),
                                value = 90, 
                                min = 0,
                                    max = 2000,
                                    step = 10),
                   
                  selectizeInput("rip_deep_organic_year", 
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
                                      selected = 1,
                                      multiple = TRUE)
                   
                   ), #bracket for well pannel 
                   

######### second one ripping with fertiliser inputs ####    

  wellPanel(
    numericInput("rip_deep_fert_cost", 
                 label = h4("Cost for ripping with deep fertiliser inputs"),
                 value = 100, 
                 min = 0,
                 max = 2000,
                 step = 10),

    selectizeInput("rip_deep_fert_year", 
                   label = h4("Ripping applied in which year? (deep with fertiliser)"),
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
        #selectInput("Results_for", 
        #      label = h3("Over how many years?"),
        #      choices = c("5 years" = 5, "10 years" = 10), 
        #      selected = "5 years"),
        
        
        radioButtons("analysis", 
                    label = h3("What analysis?"),
                    choices = c("Undiscounted annual cash flow" = "cashflow_no_dis_ann",
                                "discounted annual net cash flow" ="cashflow_dis_ann",
                                "Cummulative discounted cash flow" ="cashflow_cum_disc",
                                "Cummulative ROI not discounted" ="ROI_cum_no_disc",
                                "Cummulative ROI discounted" ="ROI_cum_disc",
                                "Benefit:Cost Ratio (discounted)" ="benefit_cost_ratio_disc",
                                "Net Present Value" ="npv"),
                                #"Modified Internal Rate Return" ="MIRR"),
                                selected = "cashflow_cum_disc") #checkboxInput bracket
        
        , #box1 bracket
        box(width=8,
        title = "Results over 5 years",
        textOutput("name_of_results"),
        plotOutput("plot")
        ))#box2 bracket
        )), #fluid bracket
        #), #tabItem bracket






     
    
    
    
    
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
  
  ####MET FILE WORK ####
  #Alex everytime I click this I get another number
  #It dosent hold onto the stationID input value
  #this is not behaving like its should
  
  
  #stationID <- reactive({
  #  isolate(input$stationID)
  #  input$refreash
  #})
 
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
  
  
  
  #### CREATING DF FOR FARM ########
  
  base_df1 <- reactive({
    map_df(input$mangement_options,
           function_base_df1, crop = input$crop_seq_zone1, 
                              discount = input$discount)
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
  
  
  final_farm_df <- reactive({
    function_final_farm_df(join_price_df())
  })
  
  #### CREATING DF FOR TREATMENTS ########
  
  #create a new df for treatments crop, yr, costs etc
  rip_noinputs_df <- reactive({
    function_rip_noinputs_df(final_farm_df(), input$year_for_ripping, input$costs_ripping)
  })
  
  rip_shallow_organic_df <- reactive({
    function_rip_shallow_organic_df(final_farm_df(), input$rip_shallow_organic_year, input$rip_shallow_organic_cost)
  })
  rip_shallow_fert_df <- reactive({
    function_rip_shallow_fert_df(final_farm_df(), input$rip_shallow_fert_year, input$rip_shallow_fert_cost)
  })
  rip_deep_organic_df <- reactive({
    function_rip_deep_organic_df(final_farm_df(), input$rip_deep_organic_year, input$rip_deep_organic_cost)
  })
  rip_deep_fert_df <- reactive({
    function_rip_deep_fert_df(final_farm_df(), input$rip_deep_fert_year, input$rip_deep_fert_cost)
  })
  rip_deep_fert1_df <- reactive({
    function_rip_deep_fert_df(final_farm_df(), input$rip_deep_fert_year, input$rip_deep_fert_cost)
  })
  wetter_df <- reactive({
    function_wetter_df(final_farm_df(), input$wetter_year, input$wetter_cost)
  })
  
  treatment_bind <- reactive({
    function_treatment_bind(rip_noinputs_df(), rip_shallow_organic_df(),rip_shallow_fert_df(), 
                            rip_deep_organic_df(), rip_deep_fert_df(), wetter_df())
  })
  
  
  #### Join Treatment df to the farm df ###
  final_treatment_farm <- reactive({
    function_final_treatment_farm(final_farm_df(), treatment_bind())
  })
  
  
  
 
  #economic indicators
  economic_indicators <- reactive({
    function_economic_indicators(final_treatment_farm())
  })
  
  plot <- reactive({
    function_plot(economic_indicators(), input$analysis )
  })
  
  
  name_plot <- reactive({
    function_metric_name(input$analysis)
  })
  
  ####### group of render OUTPUTS ########
  
  #this is new
  output$metfile <- renderTable({
    decile() 
  })
  #output$Av_yld_pot_wheat <- renderTable({
  #  decile5_yld_pot_wheat()
  #})
  #output$Av_yld_pot_pulses <- renderTable({
  #  decile5_yld_pot_pulses()
  #})
  output$yld_pot_wheat <- renderValueBox({
    valueBox(decile5_yld_pot_wheat(), "Yield potential of wheat / barley t/ha")
  })
  output$yld_pot_pulses <- renderValueBox({
    valueBox(decile5_yld_pot_pulses(), "Yield potential of pulses t/ha")
  })
  #output$metfile_file_name <- renderText({
  #   test1() 
  # })
  output$name_of_met <- renderText({
    paste0("station number: ",input$stationID)
  })

  output$name_of_results <- renderText({
    paste0("metric: ",input$analysis)
  })
 output$text_size_farm = renderText({
  paste("Total size of your farm is set to:", input$total_size_farm,"ha")
})

 ## DF for the farm ##
 output$df_progress = renderTable({
   final_farm_df()
 })
 #cost outputs
 output$df_progress_cost = renderTable({
   treatment_bind()
 })
 
 
 #final data frame
 output$df_progress_final = renderTable({
   final_treatment_farm()
 })
 
 
 ##### economic indicators #####
 output$economic = renderTable({
   economic_indicators()
 })
 output$plot = renderPlot({
   plot()
 })
 
}
shinyApp(ui, server)

