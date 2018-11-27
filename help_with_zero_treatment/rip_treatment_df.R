library(readr)
library(dplyr)
library(tidyverse)
library(readxl)

x <- data_frame(year = 1:10,
       crop = rep("wheat", 10))
y <- pre <- data.frame(year = 0)

df <- bind_rows(y, x)
glimpse(df)
#assign the cost for the year selected


ripping_cost <- 80 #from the UI
year_applied <- c(0) #from the UI

cost <- data_frame(year = as.numeric(year_applied),
                   cost = ripping_cost)
glimpse(cost)

cost_df <- left_join(df, cost, "year") #this is from csv outside
#bring in a file with the yield response over the 10 years
yld_resp_crop_treat <- read.csv("cost_rip.csv")
 cost_df <- left_join(cost_df,yld_resp_crop_treat, "year")
 
 glimpse(cost_df)
#step3 cal to modify the yield response reflecting when the treatment was applied
####Kerensa Help#####
 
 
 ####HELP NEED A CONDITION THAT SAYS DONT ADD +1 if zero
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
 
 glimpse(cost_df)
 #making temp file for a join which has a dummy yr_since_app clm
 treat <- select(cost_df, year, cost, yld_reponse_ripping)
 treat <- mutate(treat,yr_since_app = year )
 glimpse(cost_df_1)
 cost_df_1 <- left_join(cost_df, treat, by = 'yr_since_app') %>% 
   select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
          = yld_reponse_ripping.y, yr_since_app)
 
 