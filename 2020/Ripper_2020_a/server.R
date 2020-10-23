require(shiny)
require(rhandsontable)

######################################################################################################
#####                       bring in the data the app will use       #################################
######################################################################################################

df <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                 sheet = "DT_selection")
cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_yld")
extra_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                          sheet = "DT_extra_cost_benefits")




######################################################################################################
#########################                       server               #################################
######################################################################################################
server <- shinyServer(function(input, output, session) {

######################################################################################################
##################         function for filtering the data - drop down      ##########################
######################################################################################################

  
######## sc1   #######################################################################################
 
  output$data1 <- renderUI({
    selectInput("data1", "select modification",
                choices = c(unique(df$modification)),
                selected = "Ploughing")
  })
  ## input dependant on the choices in `data1`
  output$data2 <- renderUI({
    selectInput("data2", "select site",
                choices = c(unique(df$site
                                   [df$modification == input$data1])),
                selected = "Cadgee")
  })

######################################################################################################
##################         render table sc1                                    #######################
######################################################################################################

  output$tb_chosen3 <- renderTable(subset(df,
                                          df$modification==input$data1 &                                                                      df$modification==input$data2 &
                                            df$site==input$data2
  ),
  rownames=TRUE)
  
  
######################################################################################################
##################        pop up    not sure this needs to be in server        #######################
######################################################################################################
  

  shinyalert(
    title = "What the name of your farm / analysis?", 
    type = "input",
    #text = "This ia draft app",
    size = "s", 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  
######################################################################################################
##################                     reactivity                              #######################
###################################################################################################### 

######## cost table sc1 and sc2 ########
reactive_filter_cost_sc1 <- reactive({
    filter(cost_table, 
           modification == input$data1  &
             site == input$data2)   %>% 
      select(activity , price, comments, `data source`)
  })
  
reactive_filter_cost_sc2 <- reactive({
    filter(cost_table, 
           modification == input$data1  &
             site == input$data2)   %>% 
      select(activity , price, comments, `data source`)
  })
  
########   yld table sc1 and sc2   ########
reactive_filter_yld_sc1 <- reactive({
  filter(yld_table, 
         modification == input$data1  &
           site == input$data2)   %>% 
    select(year, crop, `yield  (un modified)`, `yield (modified)`,price, `data source` )
})

reactive_filter_yld_sc2 <- reactive({
  filter(yld_table, 
         modification == input$data1  &
           site == input$data2)   %>% 
    select(year, crop, `yield  (un modified)`, `yield (modified)`, price, `data source`)
})

########   extra table sc1 and sc2   ########
reactive_filter_extra_sc1 <- reactive({
  filter(extra_table, 
         modification == input$data1  &
           site == input$data2)   %>% 
    select(activity ,year, value, `data source`)
})

reactive_filter_extra_sc2 <- reactive({
  filter(extra_table, 
         modification == input$data1  &
           site == input$data2)   %>% 
    select(activity ,year, value, `data source`)
})


######   cost_sc1 yld_sc1 extra_sc1
  reactive_economics <- reactive({
    economics_tbl_sc1 <- function_economics_tb_sc1(hot_to_r(input$cost_sc1), 
                                                   hot_to_r(input$yld_sc1), 
                                                   hot_to_r(input$extra_sc1), 
                                                   1, as.double(input$years))
    economics_tbl_sc2 <- function_economics_tb_sc1(hot_to_r(input$cost_sc2), 
                                                   hot_to_r(input$yld_sc2), 
                                                   hot_to_r(input$extra_sc2), 
                                                   2, as.double(input$years))
    
    economics_tbl_sc1_sc2 <- bind_rows(economics_tbl_sc1, economics_tbl_sc2)
    economics_tbl_sc1_sc2
  })
 
  reactive_plot2 <- reactive({
    function_economics_tb_sc1(reactive_economics()) 
  })
  
#############################################################################################
####################                  functions        #####################################
#############################################################################################
  
  
  #Function 1 - that bring togther the input data table and creates a df with economics
  function_economics_tb_sc1 <- function(cost_sc_x, yld_sc_x, extra_sc_x, sc_x, run_of_years ){
    
    ## replace all na with 0 value
    cost_sc_x[is.na(cost_sc_x)] <- 0
    yld_sc_x[is.na(yld_sc_x)] <- 0
    extra_sc_x[is.na(extra_sc_x)] <- 0
    
    #value of yield
    yld_sc_x <- yld_sc_x %>%
      mutate(yld_gain_value = (`yield (modified)` - `yield  (un modified)`) * price) %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, yld_gain_value )
    
    #add a clm that has type is it a cost or saving
    extra_sc_x <- extra_sc_x %>%
      mutate(type =  case_when(
        activity == "additional costs ($/ha)" ~ "cost",
        activity == "cost harvesting and handling extra grain $/t" ~ "cost",
        activity == "additional savings ($/ha)" ~ "benefit",
        TRUE ~ activity
      ))
    
    extra_benefits_cost_sc_x <- extra_sc_x %>%
      group_by(year, type ) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>%
      ungroup()
    
    extra_benefits_sc_x <-  extra_benefits_cost_sc_x %>%
      filter(type == "benefit") %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, value )
    
    #Add the beneifits to the yld table
    benefits_sc_x <- left_join(yld_sc_x, extra_benefits_sc_x) %>%
      mutate(total_benefit = yld_gain_value + value) %>%
      select(scenario  ,  year, total_benefit )
    
    
    
    ##### now for the costs
    extra_cost_sc_x <-  extra_benefits_cost_sc_x %>%
      filter(type == "cost") %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      select(scenario, year, value ) %>%
      rename(total_cost =value)
    
    
    
    ### inital costs
    intial_cost_sc_x <- cost_sc_x %>%
      summarise(total_cost = sum(price, na.rm = TRUE)) %>%
      mutate(scenario = paste0("scenario ", sc_x)) %>%
      mutate(year = 0) %>%
      select(scenario, year, total_cost )
    
    total_cost_sc_x <- rbind(intial_cost_sc_x, extra_cost_sc_x)
    economics_tbl_sc_x <- left_join(total_cost_sc_x, benefits_sc_x)
    economics_tbl_sc_x[is.na(economics_tbl_sc_x)] <- 0
    economics_tbl_sc_x <- economics_tbl_sc_x %>%
      mutate(undiscounted_cash_flow = total_benefit - total_cost)
    
    economics_tbl_sc_x <- economics_tbl_sc_x[ 1: (run_of_years+1),]
    
    return(economics_tbl_sc_x)
  }
  
  
#############################################################################################
####################                  outputs        #####################################
############################################################################################# 
 
### cost tables
output$cost_sc1 <- renderRHandsontable({
    rhandsontable(reactive_filter_cost_sc1()) #converts the R dataframe to rhandsontable object
  })
output$cost_sc2 <- renderRHandsontable({
    rhandsontable(reactive_filter_cost_sc2()) #converts the R dataframe to rhandsontable object
  })
 

### yld tables
output$yld_sc1 <- renderRHandsontable({
  rhandsontable(reactive_filter_yld_sc1()) 
})
output$yld_sc2 <- renderRHandsontable({
  rhandsontable(reactive_filter_yld_sc2()) 
})

### yld tables
output$extra_sc1 <- renderRHandsontable({
  rhandsontable(reactive_filter_extra_sc1()) 
})
output$extra_sc2 <- renderRHandsontable({
  rhandsontable(reactive_filter_extra_sc2()) 
})

### extra tables
output$extra_sc1 <- renderRHandsontable({
  rhandsontable(reactive_filter_extra_sc1()) 
})
output$extra_sc2 <- renderRHandsontable({
  rhandsontable(reactive_filter_extra_sc2()) 
}) 

output$plot1 <- renderPlot({
  
  ggplot(data= reactive_economics(), aes(x= year, y = undiscounted_cash_flow, colour = scenario))+
    geom_line()+
    geom_hline(yintercept=0, linetype="dashed",
               color = "black", size=0.5)+
    theme_bw()+
    #scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 5, by = 1))+
    #scale_y_continuous(limits = c(y_min[[1]], y_max[[1]]))+
    xlab("Years after modification") + ylab("$/ha") +
    ggtitle("Undiscounted cash flow")
})



output$economic_tb1 <- renderPrint({   #this is just a check
  reactive_economics() #this has my filtering reactive object
})



observeEvent(input$saveBtn, write.csv(hot_to_r(input$cost),
                                        file = "test.csv",
                                        row.names = FALSE))

output$cost2 <- renderRHandsontable({
  rhandsontable(datavalues$data) #converts the R dataframe to rhandsontable object
})  

output$plot2 <- renderPlot({
  
  ggplot(data= reactive_economics(), aes(x= year, y = undiscounted_cash_flow, colour = scenario))+
    geom_line()+
    geom_hline(yintercept=0, linetype="dashed",
               color = "black", size=0.5)+
    theme_bw()+
    #scale_x_continuous(limits = c(x_min, x_max), breaks = seq(0, 5, by = 1))+
    #scale_y_continuous(limits = c(y_min[[1]], y_max[[1]]))+
    xlab("Years after modification") + ylab("$/ha") +
    ggtitle("Undiscounted cash flow")
})

observeEvent(input$preview, {
  # Show a modal when the button is pressed
  shinyalert("Inital costs include", 
             HTML("This is the first line. 
      This should be the second.
       Third line
       etc..."))
})


})
  
