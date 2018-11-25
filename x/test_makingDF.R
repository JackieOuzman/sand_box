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

function_making_df <- function(a,b,c,d){
  data.frame( wheat = a,
              barley = b,
              canola = c,
              legume = d) 
}

function_flip_df <- function(df){
  gather(df, crop, price)
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
                     step = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tableOutput("df")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  making_df <- reactive({
    function_making_df(input$a, input$b, input$c, input$d)
  })
  
  flip_df <- reactive({
    function_flip_df(making_df())
  })
   
   output$df <- renderTable({
      flip_df()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

