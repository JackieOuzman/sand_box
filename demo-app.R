#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
         
         selectInput("treatment",
                     label = "Treatment",
                     choices = list("wetting agent", "ripping")),
        
        numericInput("price_wh",
                    label = "Farm gate price wheat ($/t)",
                    value = 290,
                    min = 0,
                    max= 400,
                    step = 10),
        numericInput("price_ba",
                     label = "Farm gate price barley ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        numericInput("price_can",
                     label = "Farm gate price canola ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10),
        numericInput("price_leg",
                     label = "Farm gate price legume ($/t)",
                     value = 290,
                     min = 0,
                     max= 400,
                     step = 10)
        ),
        
                
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("crop_types"),
         tableOutput("crop_price"),
         tableOutput("df"),
         tableOutput("df1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #group all the reactive variable here
  start_df <- reactive({
    function_df_1(input$treatment, input$crop_seq_zone1)
  })  
  df <- reactive({
    fix_crop_name(start_df())
  })  
  #make the input prices reactive so I can use them in formular
  price_wheat <- reactive({
    input$price_wh
  })
  price_barley <- reactive({
    input$price_bar
  })
  
  crop_price_table <-reactive({
    function_crop_prices_df(input$price_wh, input$price_bar)
  })
  
  #df1 <- reactive({
  #  function_add_prices(crop_price_table(), test())
  #})
  
  #output renders here don't forget if calling a reactive variable it need()after the name
  
  output$crop_types <- renderText(input$price_wh)
  
  output$crop_price <- renderTable({
    crop_price_table()
  })
  output$df <- renderTable({
    df()
  })
  #output$df1 <- renderTable({
  #  df1()
  #})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

