


#devtools::install_github("AnalytixWare/ShinySky")

require(shiny)
#require(shinysky)
require(rhandsontable)



#read in the data that my app will use
df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
# yld_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
#                          sheet = "DT_yld")
# extra_cost_benefits_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
#                                           sheet = "DT_extra_cost_benefits")


######################################################################################################


##############################################################################################
#### server ##################################################################################

# Define server logic required to draw a drop down sc2
server <- shinyServer(function(input, output, session) {

######## sc1 this function renders the drop down  ########################################
  output$data1 <- renderUI({
    selectInput("data1", "select modification",
                choices = c(unique(df$modification)),
                selected = "Ploughing")
  })
  ## input dependant on the choices in `data1`
  output$data2 <- renderUI({
    selectInput("data2", "select modification",
                choices = c(unique(df$site
                            [df$modification == input$data1])),
                selected = "Cadgee")
  })
 
  
  
  ###########################################################################################

  output$tb_chosen3 <- renderTable(subset(df,
                                          df$modification==input$data1 &                                                                      df$modification==input$data2 &
                                            df$site==input$data2
  ),
  rownames=TRUE)

  
####################################################################################################  
##### reactivity ####


#reactive step for filter
reactive_filter_cost <- reactive({
     filter(cost_table, 
            modification == input$data1  &
            site == input$data2)   %>% 
     select(activity , price, comments, `data source`)
  
   })



#############################################################################################
###  functions 



######################################################################################################
#### outputs

output$cost <- renderRHandsontable({
    rhandsontable(reactive_filter_cost()) #converts the R dataframe to rhandsontable object
  })  
 
output$cost_tb1 <- renderPrint({   #this is just a check
  reactive_filter_cost() #this has my filtering reactive object
  })

observeEvent(input$saveBtn, write.csv(hot_to_r(input$cost),
                                      file = "test.csv",
                                      row.names = FALSE))

      
             
output$cost2 <- renderRHandsontable({
  rhandsontable(datavalues$data) #converts the R dataframe to rhandsontable object
})  

output$plot1 <- renderPlot({
  ggplot(data = hot_to_r(input$cost), aes(x = activity , y = price))+
    geom_col()
  
})

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
