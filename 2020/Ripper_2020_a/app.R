


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

#this clear the global env of functions and symbols
rm(list = ls(), envir = .GlobalEnv)

#source('utils_outline_idea.R') #this will be a fine that house all the functions


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Select best outcome"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        inputPanel(uiOutput("data1"),   ## uiOutput - gets the UI from the server
                   uiOutput("data2"),
                   uiOutput("data3"),
        tableOutput("tb_chosen3"),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )





################################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {

    
    df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                sheet = "DT_selection")
    
    
    
    
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
    
    
    
    
    
    
    ### old shiny
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
