library(shiny)

ui <- fluidPage(
  titlePanel("Test Dashboard "),
  sidebarLayout(
    sidebarPanel(
      uiOutput("data1"),   ## uiOutput - gets the UI from the server
      uiOutput("data2"),
      uiOutput("data3")
    ),
    mainPanel()
  ))


server <- function(input, output){
  
  Region<- c("Americas", "Asia Pacific","Asia Pacific", "EMEA", "EMEA",  "EMEA")
  Country<- c("Mexico", "China","India", "Germany", "Spain", "Spain" )
  Rating<- c("5","3","3","2","4", "1")
  book3<- data.frame(Region, Country, Rating, stringsAsFactors = F)
  
  ## renderUI - renders a UI element on the server
  ## used when the UI element is dynamic/dependant on data
  output$data1 <- renderUI({
    selectInput("data1", "Select Region", choices = c(book3$Region))
  })
  ## input dependant on the choices in `data1`
   output$data2 <- renderUI({
    selectInput("data2", "select country", choices = c(book3$Country[book3$Region == input$data1]))
   })
  ## input dependant on the choices in `data2`
  output$data3 <- renderUI({
    selectInput("data3", "select rating", choices = c(book3$Rating[book3$Country == input$data2]))
  })
}

shinyApp(ui, server)

