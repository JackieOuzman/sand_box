
library(shiny)
library(ggplot2)
library(readxl)
library(tidyverse)
library(rhandsontable)
library(shinyalert)
library(shinydashboard)


df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")






ui <- dashboardPage(
  dashboardHeader(title = ""),
  
  dashboardSidebar(disable = TRUE),
  dashboardBody(

        fluidRow(
          box(
            title = "Site", width = 4, solidHeader = TRUE, status = "primary",
          
              selectInput("site", "select",
                        choices = c(unique(df$site)),
                        selected = "Cadgee")
              
            ), #box - Site
          
           box(
             title = "Constraint", width = 8, solidHeader = TRUE, status = "primary",
          
           infoBoxOutput("non_wetting"),
           infoBoxOutput("acidic"),
           infoBoxOutput("physical"),
           valueBoxOutput("rainfall")
          
          ) #box Constraint
      ),#fluid row 1

      fluidRow(
        verbatimTextOutput(outputId = "test"),
        
      )#fluid row 2
      
    ) #dashboard body
  ) #dashboard Page
      
      
server <- function(input, output) {
#### reactivity ###
  str(df)
  reactive_df <- reactive({
    filter(df, 
           site == input$site)    %>% 
      select("non-wetting", "acidic", "physical", "rainfall_mean_annual") %>% 
        unique()
      
  })

  output$test <- renderPrint(reactive_df())  
 

output$non_wetting <- renderInfoBox({
  valueBox(value = tags$p("non-wetting", style = "font-size: 50%;"),
           subtitle = "",
           color = paste0(reactive_df()[1,1]))
})
output$acidic <- renderInfoBox({
  valueBox(value = tags$p("acidic", style = "font-size: 50%;"),
           subtitle = "",
           color = paste0(reactive_df()[1,2]))
})
output$physical <- renderInfoBox({
  valueBox(value = tags$p("physical", style = "font-size: 50%;"),
           subtitle = "",
           color = paste0(reactive_df()[1,3]))
})

output$rainfall <- renderValueBox({
   valueBox(
       value = paste0(round(reactive_df()[1,4]),2), 
       subtitle = "Annual rainfall",
       icon = icon("sun"), # not working??
       color = "blue"
  )
})
        
}       
     
      
shinyApp(ui, server)
      


#https://www.r-bloggers.com/2019/09/dynamic-ui-elements-in-shiny/