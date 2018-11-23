##this just defines my function...
function_df_2 <- function(treatment,treatment_2){
  data.frame(treatment = treatment,
             year = 1:length(treatment_2),
             crop_Type = treatment_2,
             value = rnorm(length(treatment_2), 50))
}

function_df_1 <- function(treatment,crop_seq_zone1){
  data.frame(treatment = treatment,
             year = 1:length(crop_seq_zone1),
             crop_Type = crop_seq_zone1,
             value = rnorm(length(crop_seq_zone1), 50))
}
#this function changes the df crop names to something more useful
fix_crop_name <- function(df){
  df$crop_Type <- sapply(df$crop_Type, 
                         sub, 
                         pattern = "Yr[[:digit:]]_", 
                         replacement = "")
  #sapply didnt get year 10
  df$crop_Type <- sapply(df$crop_Type, 
                         sub, 
                         pattern = "Yr10_", 
                         replacement = "")
  #change name to long one
  df$crop_Type <- ifelse(df$crop_Type == 'wh',"wheat",
                  ifelse(df$crop_Type == 'ba', "barley",
                  ifelse(df$crop_Type == 'can', "canola",
                  ifelse(df$crop_Type == 'leg', "legume",
                  ifelse(df$crop_Type == 'pas', "pasture","oops")))))
  
  return(df) 
}
