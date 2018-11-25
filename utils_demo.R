##this just defines my function...


function_df_1 <- function(treatment,crop_seq_zone1){
  data.frame(treatment = treatment,
             year = 1:length(crop_seq_zone1),
             crop = crop_seq_zone1,
             value = rnorm(length(crop_seq_zone1), 50)) 
}
#this function changes the df crop names to something more useful
fix_crop_name <- function(df){
  df$crop <- sapply(df$crop, 
                         sub, 
                         pattern = "Yr[[:digit:]]_", 
                         replacement = "")
  #sapply didnt get year 10
  df$crop <- sapply(df$crop, 
                         sub, 
                         pattern = "Yr10_", 
                         replacement = "")
  #change name to long one
  df$crop <- ifelse(df$crop == 'wh',"wheat",
                  ifelse(df$crop == 'ba', "barley",
                  ifelse(df$crop == 'can', "canola",
                  ifelse(df$crop == 'leg', "legume",
                  ifelse(df$crop == 'pas', "pasture","oops")))))
  
  return(df) 
}


#this function makes a df of the prices from the user defined prices
#input requiers input$price_wh,input$price_bar,input$price_can,input$priceleg)
#Assign the prices df to a reactive function
#as it will be used to join to base df



#Assign the prices df 
#as it will be used to join to base df
#need to do this is 2 step - prob can be better
#function_crop_prices_df <- function(price_wh, price_bar, price_can, price_leg){
#  data.frame(crop = c("wheat", "barley", "canola", "legume"),
#             price = c(price_wh, price_bar, price_can, price_leg))
#}

function_crop_prices_df <- function(price_wh, price_bar){
  data.frame(wheat = price_wh, 
             barley = price_bar,
             value = 1:2)
}

#crop_price_table <- crop_prices_df(price_wh,price_bar,price_can,price_leg)

#Assign the prices df to a reactive function
#as it will be used to join to base df

function_add_prices <- function(crop_price_table, test){
  df1 <- left_join(test, crop_price_table, by = 'crop')
  return(df1)
}

