


#devtools::install_github("AnalytixWare/ShinySky")

require(shiny)
require(shinysky)



#read in the data that my app will use
df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_yld")
extra_cost_benefits_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                          sheet = "DT_extra_cost_benefits")


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

  #################################################################################### 
  # Initiate the table for costs
  
  # the filtering will adjusted to reflect selection
   # cost_table_selection <- cost_table %>% filter(modification == "Ripping 40cm" &
   #                                                 site == "Murlong")
  
    #cost_table_selection <- reactive_filter()
 
  ## Sc1
  previous <- reactive({
      reactive_filter_cost()
    })
  
  MyChanges <- reactive({
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$hotable1))
    }
  })
  


####################################################################################################  
##### reactivity ####

## make input into reactivity so it can be used? 
mod <- reactive({
  function_cost_mod(input$data1)
})
site <- reactive({
  function_cost_site(input$data2)
})

#reactive step for filter
reactive_filter_cost <- reactive({
     filter(cost_table, modification == input$data1&
               site == input$data2)%>% 
     select(activity , price, comments, `data source`)
  
   })

#############################################################################################
###  functions 
function_cost_mod <- function(data1 ){
  mod <- paste0("modification ", data1)
  return(mod)
}
#lets make a more complicated function like the one I might use
function_cost_site <- function(data2 ){
  mod <- paste0("modification ", data2)
  return(mod)
}
  


######################################################################################################
#### outputs

output$hotable1 <- renderHotable({MyChanges()}, readOnly = F)
output$mod1 <-     renderPrint(mod())
output$site1 <-    renderPrint(site())

output$cost_tb1 <- renderPrint({
  reactive_filter_cost()
  })
output$cost_tb2 <- renderDataTable({
  reactive_filter_cost()
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
