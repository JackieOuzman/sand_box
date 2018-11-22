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
    getSiloMet_jax(input$stationID) #note the lack of ""
  })
  
  rainfall_summary <-reactive({
    cal_water_avail(input$test1)
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
    output$site <- renderText(input$stationID)
    
    output$table_crop <- renderText({
      list_of_crops_z1 <- ifelse(input$crop_seq_zone1 == 'Yr1_wh',"wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr2_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr3_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr4_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr5_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr6_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr7_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr8_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr9_wh', "wheat",
                                 ifelse(input$crop_seq_zone1 == 'Yr10_wh', "wheat",
                                  ifelse(input$crop_seq_zone1 == 'Yr1_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr2_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr3_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr4_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr5_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr6_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr7_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr8_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr9_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr10_ba', "barley",
                                  ifelse(input$crop_seq_zone1 == 'Yr1_can', "canola",
                                  ifelse(input$crop_seq_zone1 == 'Yr2_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr3_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr4_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr5_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr6_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr7_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr8_can', "canola",
                                   ifelse(input$crop_seq_zone1 == 'Yr9_can', "canola",     
                                   ifelse(input$crop_seq_zone1 == 'Yr10_can', "canola",
                                  ifelse(input$crop_seq_zone1 == 'Yr1_leg', "legume",
                                  ifelse(input$crop_seq_zone1 == 'Yr2_leg', "legume",
                                  ifelse(input$crop_seq_zone1 == 'Yr1_pas', "pasture",
                                  ifelse(input$crop_seq_zone1 == 'Yr2_pas', "pasture","oops"))))))))))
                                   ))))))))))))))))))))))))
        paste(list_of_crops_z1)
      
    })

    
    
}) #these brackets are for shiny serve at the top
  



