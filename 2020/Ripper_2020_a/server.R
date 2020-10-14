
# if (require(devtools)) install.packages("devtools")#if not already installed
# devtools::install_github("AnalytixWare/ShinySky")

#devtools::install_github("AnalytixWare/ShinySky")
#bring in the library that I will be working with
library(shiny)
library("gapminder")
library(dplyr)
library(ggplot2)
library(readxl)
library(shinysky)
library(tidyverse)

#read in the data that my app will use
df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_yld")
extra_cost_benefits_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                          sheet = "DT_extra_cost_benefits")
gapminder_df <- gapminder

sc1_2 <- read.csv("C:/Users/ouz001/working_from_home/ripper/2020/sc1_2.csv")

######################################################################################################
## function that will be used as reactive #####


function_cost_mod <- function(data2 ){
  mod <- paste0("modification ", data2)
return(mod)
}
function_cost_site <- function(data3 ){
  site <- paste0("modification ", data3)
  return(site)
}



# Define server logic required to draw a drop down
server <- shinyServer(function(input, output, session) {
  
  output$data1 <- renderUI({
    selectInput("data1", "What do you want to do to your soil?",
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
  # Initiate your table for costs
  # the filtering will adjusted to reflect selection
   cost_table_selection <- cost_table %>% filter(modification == "Ripping 40cm" &
                                                   site == "Murlong")
 
  previous <- reactive({
   cost_table_selection[1:4,4:7]})
  
  MyChanges <- reactive({
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$hotable1))
    }
  })
  

#################################################################################### 
# Initiate your table for yld
# the filtering will adjusted to reflect selection
yld_table_selection <- yld_table %>% filter(modification == "Ripping 40cm" &
                                                site == "Murlong")
previous_yld <- reactive({yld_table_selection[,4:9]})

MyChanges2 <- reactive({
  if(is.null(input$hotable2)){return(previous_yld())}
  else if(!identical(previous_yld(),input$hotable2)){
    # hot.to.df function will convert your updated table into the dataframe
    as.data.frame(hot.to.df(input$hotable2))
  }
})


#################################################################################### 
# Initiate your table for extra costs
# the filtering will adjusted to reflect selection
extra <- extra_cost_benefits_table %>% filter(modification == "Ripping 40cm" &
                                              site == "Murlong")

extra_selection <- extra %>% 
  select(-grouping, -modification, -site) %>% 
  mutate(year = paste0("year ", year)) %>% 
  pivot_wider(names_from = year, values_from = value)%>% 
  relocate(c(comments,`data source`), .after = last_col()) 

previous_extra <- reactive({extra_selection[,1:8]})

MyChanges3 <- reactive({
  if(is.null(input$hotable3)){return(previous_extra())}
  else if(!identical(previous_yld(),input$hotable3)){
    # hot.to.df function will convert your updated table into the dataframe
    as.data.frame(hot.to.df(input$hotable3))
  }
})

## call my function 
mod <- reactive({
  function_cost_mod(input$data2)
})
site <- reactive({
  function_cost_site(input$data3)
})


output$plot1 <- renderPlot({
       #filter(gapminder_df, year== input$year_plot) %>% This the bit that will be defined by user inputs
  ggplot(data= sc1_2, aes(x= year_numb, y = value, colour = scenario))+
    geom_line()+
    theme_bw()+
    xlab("Years after modification") + ylab("Undiscounted cash flow $/ha") +
    ggtitle("Scenarios")
     })

output$hotable1 <- renderHotable({MyChanges()}, readOnly = F)
#output$tbl = DT::renderDataTable(MyChanges())
output$hotable2 <- renderHotable({MyChanges2()}, readOnly = F)
output$hotable3 <- renderHotable({MyChanges3()}, readOnly = F)

output$mod1 <- renderPrint(mod())
output$site1 <- renderPrint(site())


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
