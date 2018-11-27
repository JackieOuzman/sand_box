##this just defines my function...


function_base_df1 <- function(crop_seq_zone1, discount){
  x <- data.frame(year = 1:length(crop_seq_zone1),
             crop = crop_seq_zone1,
             discount = discount)
  y <- pre <- data.frame(year = 0)
  bind_rows(y, x)
}

#this function changes the df crop names to something more useful
function_fix_crop_name <- function(base_df1){
  base_df1$crop <- sapply(base_df1$crop, 
                    sub, 
                    pattern = "Yr[[:digit:]]_", 
                    replacement = "")
  #sapply didnt get year 10
  base_df1$crop <- sapply(base_df1$crop, 
                    sub, 
                    pattern = "Yr10_", 
                    replacement = "")
  #change name to long one
  base_df1$crop <- ifelse(base_df1$crop == 'wh',"wheat",
              ifelse(base_df1$crop == 'ba', "barley",
               ifelse(base_df1$crop == 'can', "canola",
               ifelse(base_df1$crop == 'leg', "legume",
               ifelse(base_df1$crop == 'pas', "pasture","oops")))))
  
  return(base_df1) 
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
library(dplyr)
function_flip_df_current <- function(making_df_current){
  gather(making_df_current, crop, current_yld)
}
#now join it to the df
function_join_current_df <- function(fix_crop_name, flip_df_current){
  left_join(fix_crop_name, flip_df_current, by = 'crop')
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
function_flip_df_potential <- function(making_df_potential){
  gather(making_df_potential, crop, potential_yld)
}
#now join it to the df
function_join_potential_df <- function(join_current_df, flip_df_potential){
  left_join(join_current_df, flip_df_potential, by = 'crop')
}

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
function_flip_df_price <- function(making_df_price){
  gather(making_df_price, crop, price)
}

#now join it to the df
function_join_price_df <- function(join_potential_df, flip_df_price){
  left_join(join_potential_df, flip_df_price, by = 'crop')
}

#Now make a sep df for treatments with crops, year costs and yield response
function_treatments_df <- function(join_price_df, year_for_ripping, costs_ripping){
  #a <- data_frame(select(join_price_df, year, crop))
  a <- join_price_df
  b <- data_frame(year = as.numeric(year_for_ripping), #as numeric
                  cost = costs_ripping)
  cost_df <- left_join(a, b, "year")
  cost_df <- select(cost_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("cost_rip.csv")
  cost_df <- left_join(cost_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  cost_df <- cost_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  cost_df$year <- as.integer(cost_df$year)
  cost_df <- cost_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)+1) %>% 
    select(year, cost, yld_reponse_ripping, crop, yr_since_app)
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(cost_df, year, cost, yld_reponse_ripping)
  treat <- mutate(treat,yr_since_app = year )
  cost_df <- left_join(cost_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse_ripping.y, yr_since_app)
  
  
}
