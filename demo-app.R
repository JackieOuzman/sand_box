#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

make_data_frame <- function(treatment, crop_type) {
  out_df <- data.frame(treatment = treatment,
                       year = 1:length(crop_type),
                       crop_type = crop_type,
                       value = rnorm(length(crop_type), 50))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("crop_seq",
                        label = "Crop type",
                        choices = list("wheat", "barley", "canole", "legume", "pasture")),
         selectInput("treatment",
                     label = "Treatment",
                     choices = list("wetting agent", "ripping")),
         actionButton("addButton", "Update")),
                        
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("crop_types"),
         tableOutput("df")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues()
  values$crop_types <- vector(mode = "character", length = 0)
  values$treatment <- vector(mode = "character", length = 0)
  # values$df <- 
  
  observe({
    if(input$addButton > 0) {
      values$crop_types <- isolate(c(values$crop_types, input$crop_seq))
      values$treatment <- isolate(c(values$treatment, input$treatment))
      values$df <- isolate(purrr::map2_df(values$treatment,
                                          values$crop_type,
                                          make_data_frame))
    }
  })
  
  output$crop_types <- renderText(values$crop_types)
  
  output$df <- renderTable(values$df)
}

# Run the application 
shinyApp(ui = ui, server = server)

