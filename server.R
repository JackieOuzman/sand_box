library(shiny)
library(dplyr)
shinyServer(function(input, output) {
### Reactive data frames #####
  
    df <- reactive({
    data.frame(year = 1:10,site = input$stationID)
    })
  
  df_treat <- reactive({
    (data.frame(treat = input$mangement_options))
    })
  df_depth <- reactive({
     (data.frame(depth = input$depth)) 
    })
  
  test1 <- reactive({
    getSiloMet_jax(input$stationID) #this is the first function
  })
  
  rainfall_summary <-reactive({
    cal_water_avail(test1())
  })  
  
  deciles_test <-reactive({
    decile(rainfall_summary())
  }) 
  dec_5_yld_pot <-reactive({
    decile5_yld_pot(rainfall_summary())
  }) 
  
  list_of_crops_z1df <-reactive({  
  grep_pass1 <- sub('Yr[[:digit:]]_', '', input$crop_seq_zone1)
  print(grep_pass1)
  grep_pass2 <- sub('Yr10_', '', grep_pass1)
  print(grep_pass2)
  list_of_crops_z1 <- ifelse(grep_pass2 == 'wh',"wheat",
                      ifelse(grep_pass2 == 'ba', "barley",
                      ifelse(grep_pass2 == 'can', "canola",
                      ifelse(grep_pass2 == 'leg', "legume",
                      ifelse(grep_pass2 == 'pas', "pasture","oops")))))
  list_crops_z1df<- list_of_crops_z1
  })

####Display outputs####  
###Some reactive df  
    output$table1 <- renderTable({
    df()
    })
    output$table2 <- renderTable({
    df_treat()
    })
    output$table3 <- renderTable({
    df_depth()
    })
    output$test <- renderTable({
      rainfall_summary()
    }) 
    output$deciles_table <- renderTable({
      deciles_test()
    }) 
    output$xxx <- renderTable({
      dec_5_yld_pot()
    }) 
    output$site <- renderText(input$stationID)
    
    output$what_is <- renderText(data.frame(input$crop_seq_zone1))
      
      
      output$table_crop <- renderText({
        paste(list_of_crops_z1df())
      
    })

    
    
}) #these brackets are for shiny serve at the top
  



