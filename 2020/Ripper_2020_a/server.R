
#bring in the library that I will be working with
library(shiny)
library("gapminder")
library(dplyr)
library(ggplot2)
library(readxl)
library(shinysky)

#read in the data that my app will use
df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
gapminder_df <- gapminder


# Define server logic required to draw a drop down
server <- shinyServer(function(input, output, session) {
  
  output$data1 <- renderUI({
    selectInput("data1", "Select grouping",
                choices = c(df$grouping))
  })
  ## input dependant on the choices in `data1`
  output$data2 <- renderUI({
    selectInput("data2", "select modification",
                choices = c(df$modification
                            [df$grouping == input$data1]))
  })
  ## input dependant on the choices in `data2`
  output$data3 <- renderUI({
    selectInput("data3", "select site",
                choices = c(df$site[df$modification == input$data2]))
  })

  output$tb_chosen3 <- renderTable(subset(df,
                                          df$grouping==input$data1 &                                                                      df$modification==input$data2 &
                                            df$site==input$data3
  ),
  rownames=TRUE)

  #################################################################################### 
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
  
 ######################################################################## 
# ### This is old part of app that is not plugged in  
#   output$year_plot <- renderText({
#     paste0("Year selected for plotting:", input$year_plot)
#   })
#   
#   
#   
#   output$scatterplot1 <- renderPlot({
#     filter(gapminder_df, year== input$year_plot) %>% 
#       ggplot(aes(gdpPercap, lifeExp, colour= continent))+
#       geom_point()+
#       labs(x= "GPP per Capita",
#            y = "Life expectancy")
#   })
#   
#   #reactive step for filter
#   reactive_filter <- reactive({
#     filter(gapminder_df, year== input$year_plot)
#   })
#   #second plot using reactive step for filter     
#   output$scatterplot2 <- renderPlot({
#     ggplot(reactive_filter(), aes(pop, lifeExp, colour= continent))+
#       geom_point()+
#       labs(x= "Population",
#            y = "Life expectancy")
#   })
#   
#   
#   
# 
# # Run the application 
# #shinyApp(ui = ui, server = server)
