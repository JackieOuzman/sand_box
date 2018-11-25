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

#making a data frame of the prices two step process
#make a data frame and then flip it using gather

#step 1
function_making_df_price <- function(a,b,c,d){
  data.frame( wheat = a,
              barley = b,
              canola = c,
              legume = d) 
}
#step 2
function_flip_df_price <- function(df){
  gather(df, crop, price)
}

#now join it to the df
function_join_price_df <- function(df, flip_df_price){
  left_join(df, flip_df_price, by = 'crop')
}


#making a data frame of the current yield two step process
#make a data frame and then flip it using gather

#step 1
function_making_df_current <- function(aa,bb,cc,dd){
  data.frame( wheat = aa,
              barley = bb,
              canola = cc,
              legume = dd) 
}
#step 2
function_flip_df_current <- function(df){
  gather(df, crop, current_yld)
}
#now join it to the df
function_join_current_df <- function(df, flip_df_current){
  left_join(df, flip_df_current, by = 'crop')
}

#making a data frame of the potential yield two step process
#make a data frame and then flip it using gather

#step 1
function_making_df_potential <- function(aaa,bbb,ccc,ddd){
  data.frame( wheat = aaa,
              barley = bbb,
              canola = ccc,
              legume = ddd) 
}
#step 2
function_flip_df_potential <- function(df){
  gather(df, crop, potential_yld)
}
#now join it to the df
function_join_potential_df <- function(df, flip_df_potential){
  left_join(df, flip_df_potential, by = 'crop')
}
