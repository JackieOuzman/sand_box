#devtools::install_github("AnalytixWare/ShinySky") 

rm(list = ls())
library(shiny)
library(shinysky)

server <- shinyServer(function(input, output, session) {
  
  # Initiate your table
  previous <- reactive({mtcars[1:10,]})
  
  MyChanges <- reactive({
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$hotable1))
    }
  })
  output$hotable1 <- renderHotable({MyChanges()}, readOnly = F)
  output$tbl = DT::renderDataTable(MyChanges())
})

#ui <- basicPage(mainPanel(column(6,hotable("hotable1")),column(6,DT::dataTableOutput('tbl')))) #this is working
 # ui <- fluidPage(
 #   titlePanel("test fluid"),
 #   sidebarLayout(
 #     sidebarPanel(hotable("hotable1")),
 #     #sidebarPanel(hotable("hotable1")),
 #     mainPanel(DT::dataTableOutput('tbl')
 #     )
 #   )
 # )
 # 
       
ui <- fluidPage(
  titlePanel("test fluid"),
    mainPanel(hotable("hotable1"),
              mainPanel(hotable("hotable1"))
    
  )
)


shinyApp(ui, server)