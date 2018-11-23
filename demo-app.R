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
   titlePanel("Old Faithful Geyser Data"),
   
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
                       options = list(maxItems = 10),
                       multiple = TRUE
        ),
         
         selectInput("treatment",
                     label = "Treatment",
                     choices = list("wetting agent", "ripping")),
        selectInput("treatment_2",
                    label = "Treatment2",
                    choices = list("wetting agent2", "ripping2"))
        ),
        
                
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("crop_types"),
         tableOutput("df")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  test <- reactive({
    function_df_1(input$treatment, input$crop_seq_zone1)
  })  
  
  output$crop_types <- renderText(input$treatment_2)
  
  output$df <- renderTable({
    test()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

