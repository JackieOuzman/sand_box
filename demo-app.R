#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)

source('utils_demo.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Happy Happy Demo App"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
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
         
        checkboxGroupInput("treatment",
                     label = "Treatment",
                     choices = list("wetting agent", "ripping"),
                     selected = "ripping" ),
        
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
                     step = 10),
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
                     step = 0.5),
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
                     step = 0.5),
        numericInput("discount",
                     label = "Discount factor",
                     value = 0.6,
                     min = 0,
                     max= 1,
                     step = 0.02)
        ),
        
                
      # Show a df tables I am making...
      mainPanel(
        tableOutput("df"), 
        tableOutput("df_with_price")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #group all the reactive variable here
  start_df <- reactive({
    function_df_1(input$treatment, input$crop_seq_zone1, input$discount)
  })  
  df <- reactive({
    fix_crop_name(start_df())
  })  
  #making a data frame of the prices two step process
  #make a data frame and then flip it using gather
  making_df_price <- reactive({
    function_making_df_price(input$a, input$b, input$c, input$d)
  })
  flip_df_price <- reactive({
    function_flip_df_price(making_df_price())
  })
  join_price_df <- reactive({
    function_join_price_df(df(),flip_df_price())
  })
  
  #making a data frame of the current yld two step process
  #make a data frame and then flip it using gather
  making_df_current <- reactive({
    function_making_df_current(input$aa, input$bb, input$cc, input$dd)
  })
  flip_df_current <- reactive({
    function_flip_df_current(making_df_current())
  })
  join_current_df <- reactive({
    function_join_current_df(join_price_df(),flip_df_current())
  })
  
  #making a data frame of the potential yld two step process
  #make a data frame and then flip it using gather
  making_df_potential <- reactive({
    function_making_df_current(input$aaa, input$bbb, input$ccc, input$ddd)
  })
  flip_df_potential <- reactive({
    function_flip_df_potential(making_df_potential())
  })
  join_potential_df <- reactive({
    function_join_potential_df(join_current_df(),flip_df_potential())
  })
  
  #output renders here don't forget if calling a reactive variable it need()after the name
  
  
  #output$df <- renderTable({
  #  df()
  #})
  output$df_with_price <- renderTable({
    join_potential_df()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

