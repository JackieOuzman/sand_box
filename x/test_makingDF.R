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
library(dplyr)
#load data_cost
#data <- read.csv("cost_rip.csv")

#base data frame
function_df_yr <- function(treatment,crop_seq_zone1){
  data.frame(treatment = treatment,
             year = 1:length(crop_seq_zone1),
             crop = crop_seq_zone1) 
}

#this function changes the df crop names to something more useful
fix_crop_name <- function(df){
  df$crop <- sapply(df$crop, 
                    sub, 
                    pattern = "Yr[[:digit:]]_", 
                    replacement = "")
  #sapply didnt get year 10
  df$crop <- sapply(df$crop, 
                    sub, 
                    pattern = "Yr10_", 
                    replacement = "")
  #change name to long one
  df$crop <- ifelse(df$crop == 'wh',"wheat",
             ifelse(df$crop == 'ba', "barley",
             ifelse(df$crop == 'can', "canola",
             ifelse(df$crop == 'leg', "legume",
             ifelse(df$crop == 'pas', "pasture","oops")))))
  
  return(df) 
}
#function for prices df
function_making_df <- function(a,b,c,d){
  data.frame( wheat = a,
              barley = b,
              canola = c,
              legume = d) 
}

function_flip_df <- function(df){
  gather(df, crop, price)
}
#now join it to the df
function_join_price_df <- function(df, flip_df_price){
  left_join(df, flip_df_price, by = 'crop')
}

#now join it to the df to the imported costs table
function_join_rip_cost_df <- function(df, rip_cost){
  left_join(df, rip_cost, by = 'year')
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("prices df"),
   
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("treatment",
                    label = "Treatment",
                    choices = list("wetting agent", "ripping")),
        
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
        )
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("df_price"),
         tableOutput("df1"),
         tableOutput("rip_cost"),
         tableOutput("df_final")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #price step1
  making_df_price <- reactive({
    function_making_df(input$a, input$b, input$c, input$d)
  })
  #price step2
  flip_df_price <- reactive({
    function_flip_df(making_df_price())
  })
  #df step1
  start_df <- reactive({
    function_df_yr(input$treatment, input$crop_seq_zone1)
  })  
  #df step2
  df1 <- reactive({
    fix_crop_name(start_df())
  })  
  #join the two tables price and base
  df <- reactive({
    function_join_price_df(df1(), flip_df_price())
  })  
  
  #join rip_cost and base df - note rip_cost is loaded at the top
  #we need to load the data based on treatment type... could have it in one file??
   
   
  #load in the data - dont think this is best method??
  data <- reactive({
    data <- read.csv("cost_rip.csv")
  })
  df_final <- reactive({
    function_join_rip_cost_df(df(), data())
  }) 
  
  
  
  #rendering rip_cots
  output$rip_cost <- renderTable({
    data()
  })
  
  
  
  output$df_price <- renderTable({
      flip_df_price()
   })
   output$df1 <- renderTable({
     df()
   })
   output$df1 <- renderTable({
     df_final()
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

