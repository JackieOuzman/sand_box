


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


sc1_2 <- read.csv("C:/Users/ouz001/working_from_home/ripper/2020/sc1_2.csv") #won't need this later

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

##############################################################################################
#### server ##################################################################################

# Define server logic required to draw a drop down sc2
server <- shinyServer(function(input, output, session) {

######## sc1 this function renders the drop down  ########################################
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
  
  
  ######## sc2 this function renders the drop down ? do we want 2 ###################################
  output$data1_sc2 <- renderUI({
    selectInput("data1_sc2", "What do you want to do to your soil?",
                choices = c(df$grouping))
  })
  ## input dependant on the choices in `data1`
  output$data2_sc2 <- renderUI({
    selectInput("data2_sc2", "select modification",
                choices = c(df$modification
                            [df$grouping == input$data1_sc2]))
  })
  ## input dependant on the choices in `data2`
  output$data3_sc2 <- renderUI({
    selectInput("data3_sc2", "select site",
                choices = c(df$site[df$modification == input$data2_sc2]))
  })
  


  output$tb_chosen3 <- renderTable(subset(df,
                                          df$grouping==input$data1 &                                                                      df$modification==input$data2 &
                                            df$site==input$data3
  ),
  rownames=TRUE)

  #################################################################################### 
  # Initiate the table for costs
  
  # the filtering will adjusted to reflect selection
   cost_table_selection <- cost_table %>% filter(modification == "Ripping 40cm" &
                                                   site == "Murlong")
 ## Sc1
  previous <- reactive({
   cost_table_selection[1:4,4:7]})
  
  MyChanges <- reactive({
    if(is.null(input$hotable1)){return(previous())}
    else if(!identical(previous(),input$hotable1)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$hotable1))
    }
  })
  ## sc2
  previous_sc2<- reactive({
    cost_table_selection[1:4,4:7]})
  
  MyChanges_sc2 <- reactive({
    if(is.null(input$hotable1a)){return(previous_sc2())}
    else if(!identical(previous_sc2(),input$hotable1a)){
      # hot.to.df function will convert your updated table into the dataframe
      as.data.frame(hot.to.df(input$hotable1a))
    }
  })

#################################################################################### 
# Initiate your table for yld sc1
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

# Initiate your table for yld sc1
# the filtering will adjusted to reflect selection
yld_table_selection <- yld_table %>% filter(modification == "Ripping 40cm" &
                                              site == "Murlong")
previous_yld_sc2 <- reactive({yld_table_selection[,4:9]})

MyChanges2_sc2 <- reactive({
  if(is.null(input$hotable2a)){return(previous_yld_sc2())}
  else if(!identical(previous_yld_sc2(),input$hotable2a)){
    # hot.to.df function will convert your updated table into the dataframe
    as.data.frame(hot.to.df(input$hotable2a))
  }
})

#################################################################################### 
# Initiate your table for extra costs 
# the filtering will adjusted to reflect selection
extra <- extra_cost_benefits_table %>% filter(modification == "Ripping 40cm" &
                                              site == "Murlong")
#this step changes the format to display the data
extra_selection <- extra %>% 
  select(-grouping, -modification, -site) %>% 
  mutate(year = paste0("year ", year)) %>% 
  pivot_wider(names_from = year, values_from = value)%>% 
  relocate(c(comments,`data source`), .after = last_col()) 

previous_extra <- reactive({extra_selection[,1:8]})
## sc1
MyChanges3 <- reactive({
  if(is.null(input$hotable3)){return(previous_extra())}
  else if(!identical(previous_extra(),input$hotable3)){
    # hot.to.df function will convert your updated table into the dataframe
    as.data.frame(hot.to.df(input$hotable3))
  }
})
## sc2
previous_extra <- reactive({extra_selection[,1:8]})
MyChanges3_sc2 <- reactive({
  if(is.null(input$hotable3a)){return(previous_extra())}
  else if(!identical(previous_extra(),input$hotable3a)){
    # hot.to.df function will convert your updated table into the dataframe
    as.data.frame(hot.to.df(input$hotable3a))
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
output$hotable1a <- renderHotable({MyChanges_sc2()}, readOnly = F)
output$hotable2 <- renderHotable({MyChanges2()}, readOnly = F)
output$hotable2a <- renderHotable({MyChanges2_sc2()}, readOnly = F)
output$hotable3 <- renderHotable({MyChanges3()}, readOnly = F)
output$hotable3a <- renderHotable({MyChanges3_sc2()}, readOnly = F)

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
