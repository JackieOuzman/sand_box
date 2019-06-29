##this just defines my function...
#this get the downlaoded met files they are in a folder called met
#I was thinking this would be quicker than using the uRL function I had before
#But it is just as slow:(
#
#### libraries ####
library(reshape2)
library(ggplot2)
library(dplyr)
#### file wide vrbls ####
doDbg = FALSE


#### met function block ####
function_met <- function(stationID) {
  met_file <- read_csv(paste0("met_file/",stationID,".csv"))
    return(met_file)
}

#This cal the available water based on downloaded met file

#input is a met df with daily rainfall data and output is ....

function_water_aval <- function(met_file) {
  rainfall_1 <- select(met_file, date, daily_rain, station)
  #Create new clm month and year
  rainfall_1$month <- as.Date(rainfall_1$date, "%d/%m/%Y")
  rainfall_1$year <- as.Date(rainfall_1$date, "%d/%m/%Y")
  
  rainfall_1$month <- months.Date(rainfall_1$month)
  rainfall_1$year <- year(rainfall_1$year)
  
  #create new clm that is month and year
  rainfall_1$Month_Yr <- as.character(rainfall_1$date, format="%b-%Y")
  #recode the months into summer rainfall or GS rainfall
  rainfall_1$rain_season <- recode(rainfall_1$month , "January" = "summer", 
                                   "February" = "summer",
                                   "March" = "summer",
                                   "April" = "GS",
                                   "May" = "GS",
                                   "June" = "GS",
                                   "July" = "GS",
                                   "August" = "GS",
                                   "September" = "GS",
                                   "October" = "GS",
                                   "November" = "summer",
                                   "December" = "summer")
  
  #use aggreate function to sum the rain clm by year and rain season making a new df
  sum_yr_rain_season <- aggregate(daily_rain~ year+rain_season, data = rainfall_1, FUN= sum)
  
  
  #cal new clm with summer rainfall to be *0.25
  sum_yr_rain_season$Rain_GS_summer <- 
    with(sum_yr_rain_season, 
         ifelse(rain_season == 'summer', daily_rain*0.25,daily_rain))
  
  #use aggreate function to sum the summer_rain0.25 clm by year making a new df
  water_aval <- aggregate(Rain_GS_summer~ year, sum_yr_rain_season, FUN= sum)
  
  water_aval <- mutate(water_aval,FS_yld_pot_wheat = ((((Rain_GS_summer - 60)*22)*1.12)/1000),
                       FS_yld_pot_pulses = ((((Rain_GS_summer - 60)*16)*1.12)/1000)) 
  
  return(water_aval)
}

#function to cal the DECILE YEARS for a met file
#take as met file that is processed to have aviliable water clm
#and output a table of deciles
#library(dplyr)
function_decile <- function(water_aval) {
  yield_pot1 <- water_aval %>%   
    mutate(test_name = percent_rank(Rain_GS_summer))
  yield_pot1$test_name <- round(yield_pot1$test_name,2)
  
  #Assign label to df for decile
  yield_pot1 <- yield_pot1 %>% 
    mutate(Decile = ifelse(test_name < 0.1, "Decile1",
                    ifelse(test_name >= 0.1 & test_name <= 0.2, "Decile2",
                    ifelse(test_name >= 0.2 & test_name <= 0.3, "Decile3",
                    ifelse(test_name >= 0.3 & test_name <= 0.4, "Decile4",                         
                    ifelse(test_name >= 0.4 & test_name <= 0.5, "Decile5",                         
                    ifelse(test_name >= 0.5 & test_name <= 0.6, "Decile6",                         
                    ifelse(test_name >= 0.6 & test_name <= 0.7, "Decile7",                        
                    ifelse(test_name >= 0.7 & test_name <= 0.8, "Decile8",      
                    ifelse(test_name >= 0.8 & test_name <= 0.9, "Decile9","Decile10"))))))))))      
  
  Analogue_yrs <- yield_pot1 %>% 
    group_by(Decile) %>% 
    summarize(Year = paste(sort(unique(year)),collapse=", "))
  
  Analogue_yrs$Decile <- factor(Analogue_yrs$Decile, c("Decile1", 
                                                       "Decile2", 
                                                       "Decile3", 
                                                       "Decile4", 
                                                       "Decile5",
                                                       "Decile6",
                                                       "Decile7",
                                                       "Decile8",
                                                       "Decile9",
                                                       "Decile10"))
  
  
  Analogue_yrs <- arrange(Analogue_yrs, xtfrm(Decile))
  
  return(Analogue_yrs)
}

function_decile5_yld_pot_wheat <- function(water_aval) {
  yield_pot1 <- water_aval %>%  
    mutate(test_name = percent_rank(Rain_GS_summer))
  yield_pot1$test_name <- round(yield_pot1$test_name,2)
  
  #Assign label to df for decile
  yield_pot1 <- yield_pot1 %>% 
    mutate(Decile = ifelse(test_name < 0.1, "Decile1",
                    ifelse(test_name >= 0.1 & test_name <= 0.2, "Decile2",
                    ifelse(test_name >= 0.2 & test_name <= 0.3, "Decile3",
                    ifelse(test_name >= 0.3 & test_name <= 0.4, "Decile4",                         
                    ifelse(test_name >= 0.4 & test_name <= 0.5, "Decile5",                         
                    ifelse(test_name >= 0.5 & test_name <= 0.6, "Decile6",                         
                    ifelse(test_name >= 0.6 & test_name <= 0.7, "Decile7",                        
                    ifelse(test_name >= 0.7 & test_name <= 0.8, "Decile8",      
                    ifelse(test_name >= 0.8 & test_name <= 0.9, "Decile9","Decile10"))))))))))      
  
  
  decile_av <- yield_pot1 %>% 
    group_by(Decile) %>%
    summarise(av_yld_pot= round(mean(FS_yld_pot_wheat),2),
              stdev_yld_pot = round(sd(FS_yld_pot_wheat),2))
  decile_5 <- subset(decile_av, Decile == "Decile5")
  
  return(decile_5[[1,2]])
}


function_decile5_yld_pot_pulses <- function(water_aval) {
  yield_pot1 <- water_aval %>%  
    mutate(test_name = percent_rank(Rain_GS_summer))
  yield_pot1$test_name <- round(yield_pot1$test_name,2)
  
  #Assign label to df for decile
  yield_pot1 <- yield_pot1 %>% 
    mutate(Decile = ifelse(test_name < 0.1, "Decile1",
                    ifelse(test_name >= 0.1 & test_name <= 0.2, "Decile2",
                    ifelse(test_name >= 0.2 & test_name <= 0.3, "Decile3",
                    ifelse(test_name >= 0.3 & test_name <= 0.4, "Decile4",                         
                    ifelse(test_name >= 0.4 & test_name <= 0.5, "Decile5",                         
                    ifelse(test_name >= 0.5 & test_name <= 0.6, "Decile6",                         
                    ifelse(test_name >= 0.6 & test_name <= 0.7, "Decile7",                        
                    ifelse(test_name >= 0.7 & test_name <= 0.8, "Decile8",      
                                                                            ifelse(test_name >= 0.8 & test_name <= 0.9, "Decile9","Decile10"))))))))))      
  
  
  decile_av <- yield_pot1 %>% 
    group_by(Decile) %>%
    summarise(av_yld_pot= round(mean(FS_yld_pot_pulses),2),
              stdev_yld_pot = round(sd(FS_yld_pot_pulses),2))
  decile_5 <- subset(decile_av, Decile == "Decile5")
  #write.csv(decile_5, "decile_5.csv")
  return(decile_5[[1,2]])
}


#### MAKING A DF FOR THE FARM ZONE1 ######

#this is working but the year 0 has no treatment assigned
function_base_df1 <- function(mangement_options,crop_seq_zone1, discount){
  x <- data.frame(treatment = mangement_options,
                  year = 1:length(crop_seq_zone1),
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
  base_farm <-left_join(join_potential_df, flip_df_price, by = 'crop')
  #write.csv(base_farm, "base_farm.csv")
  return(base_farm)
 }
function_final_farm_df <- function(join_price_df){
  join_price_df <- fill(join_price_df, treatment,.direction = c("up"))
  join_price_df <- unite(join_price_df, ID,
                         c(year,treatment), remove = FALSE)
  #write.csv(join_price_df, "final_farm_df.csv")
  return(join_price_df)
  
}


###### DF FOR TREATMENTS ########
##New plan is to make treatment df for each of the treatments we have ###
## Then join to the final_farm_df the missing treatments wont join###

#Now make a sep df for treatments Ripping with no inputs first
function_rip_noinputs_df <- function(final_farm_df, year_for_ripping, costs_ripping){
  a <-  distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(year_for_ripping), #as numeric
                  cost = costs_ripping) #,
                  #treatment = "rip_no_input")
  cost_rip_noinput_df <- left_join(a, b, "year")
  cost_rip_noinput_df <- select(cost_rip_noinput_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_no_input")
  cost_rip_noinput_df <- left_join(cost_rip_noinput_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  cost_rip_noinput_df <- cost_rip_noinput_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  cost_rip_noinput_df$year <- as.integer(cost_rip_noinput_df$year)
  cost_rip_noinput_df <- cost_rip_noinput_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
 #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(cost_rip_noinput_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  cost_rip_noinput_df <- left_join(cost_rip_noinput_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  cost_rip_noinput_df <- left_join(cost_rip_noinput_df, yld_resp_crop_rip, by = 'crop')
  cost_rip_noinput_df <- mutate(cost_rip_noinput_df, treatment = "rip_no_inputs")
  cost_rip_noinput_df <- unite(cost_rip_noinput_df, ID,
                         c(year,treatment), remove = FALSE)
  
  #write.csv(cost_rip_noinput_df, "cost_rip_noinput_df.csv")
  return(cost_rip_noinput_df)
  }


#Now make a sep df for treatments Ripping with shallow organic inputs 
function_rip_shallow_organic_df <- function(final_farm_df, rip_shallow_organic_year, rip_shallow_organic_cost){
  a <- distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(rip_shallow_organic_year), #as numeric
                  cost = rip_shallow_organic_cost) #,
  #treatment = "rip_no_input")
  rip_shallow_organic_df <- left_join(a, b, "year")
  rip_shallow_organic_df <- select(rip_shallow_organic_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_shallow_organic")
  rip_shallow_organic_df <- left_join(rip_shallow_organic_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  rip_shallow_organic_df <- rip_shallow_organic_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  rip_shallow_organic_df$year <- as.integer(rip_shallow_organic_df$year)
  rip_shallow_organic_df <- rip_shallow_organic_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
  
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(rip_shallow_organic_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  rip_shallow_organic_df <- left_join(rip_shallow_organic_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  rip_shallow_organic_df <- left_join(rip_shallow_organic_df, yld_resp_crop_rip, by = 'crop')
  rip_shallow_organic_df <- mutate(rip_shallow_organic_df, treatment = "rip_shallow_organic")
  rip_shallow_organic_df <- unite(rip_shallow_organic_df, ID,
                               c(year,treatment), remove = FALSE)
  #write.csv(rip_shallow_organic_df, "rip_shallow_organic_df.csv")
  return(rip_shallow_organic_df)
}


#Now make a sep df for treatments Ripping with shallow fertliser inputs 
function_rip_shallow_fert_df <- function(final_farm_df, rip_shallow_fert_year, rip_shallow_fert_cost){
  a <- distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(rip_shallow_fert_year), #as numeric
                  cost = rip_shallow_fert_cost) #,
  #treatment = "rip_no_input")
  rip_shallow_fert_df <- left_join(a, b, "year")
  rip_shallow_fert_df <- select(rip_shallow_fert_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_shallow_fert")
  rip_shallow_fert_df <- left_join(rip_shallow_fert_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  rip_shallow_fert_df <- rip_shallow_fert_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  rip_shallow_fert_df$year <- as.integer(rip_shallow_fert_df$year)
  rip_shallow_fert_df <- rip_shallow_fert_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
  
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(rip_shallow_fert_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  rip_shallow_fert_df <- left_join(rip_shallow_fert_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  rip_shallow_fert_df <- left_join(rip_shallow_fert_df, yld_resp_crop_rip, by = 'crop')
  rip_shallow_fert_df <- mutate(rip_shallow_fert_df, treatment = "rip_shallow_fert")
  rip_shallow_fert_df <- unite(rip_shallow_fert_df, ID,
                                  c(year,treatment), remove = FALSE)
  #write.csv(rip_shallow_fert_df, "rip_shallow_fert_df.csv")
  return(rip_shallow_fert_df)
}


#Now make a sep df for treatments Ripping with deep organic inputs 
function_rip_deep_organic_df <- function(final_farm_df, rip_deep_organic_year, rip_deep_organic_cost){
  a <- distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(rip_deep_organic_year), #as numeric
                  cost = rip_deep_organic_cost) #,
  #treatment = "rip_no_input")
  rip_deep_organic_df <- left_join(a, b, "year")
  rip_deep_organic_df <- select(rip_deep_organic_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_deep_organic")
  rip_deep_organic_df <- left_join(rip_deep_organic_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  rip_deep_organic_df <- rip_deep_organic_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  rip_deep_organic_df$year <- as.integer(rip_deep_organic_df$year)
  rip_deep_organic_df <- rip_deep_organic_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
  
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(rip_deep_organic_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  rip_deep_organic_df <- left_join(rip_deep_organic_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  rip_deep_organic_df <- left_join(rip_deep_organic_df, yld_resp_crop_rip, by = 'crop')
  rip_deep_organic_df <- mutate(rip_deep_organic_df, treatment = "rip_deep_organic")
  rip_deep_organic_df <- unite(rip_deep_organic_df, ID,
                               c(year,treatment), remove = FALSE)
  #write.csv(rip_deep_organic_df, "rip_deep_organic_df.csv")
  return(rip_deep_organic_df)
}


#Now make a sep df for treatments Ripping with deep fert inputs 
function_rip_deep_fert_df <- function(final_farm_df, rip_deep_fert_year, rip_deep_fert_cost){
  a <- distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(rip_deep_fert_year), #as numeric
                  cost = rip_deep_fert_cost) #,
  #treatment = "rip_no_input")
  rip_deep_fert_df <- left_join(a, b, "year")
  rip_deep_fert_df <- select(rip_deep_fert_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_deep_fert")
  rip_deep_fert_df <- left_join(rip_deep_fert_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  rip_deep_fert_df <- rip_deep_fert_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  rip_deep_fert_df$year <- as.integer(rip_deep_fert_df$year)
  rip_deep_fert_df <- rip_deep_fert_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
  
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(rip_deep_fert_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  rip_deep_fert_df <- left_join(rip_deep_fert_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  rip_deep_fert_df <- left_join(rip_deep_fert_df, yld_resp_crop_rip, by = 'crop')
  rip_deep_fert_df <- mutate(rip_deep_fert_df, treatment = "rip_deep_fert")
  rip_deep_fert_df <- unite(rip_deep_fert_df, ID,
                               c(year,treatment), remove = FALSE)
  #write.csv(rip_deep_fert_df, "rip_deep_fert_df.csv")
  return(rip_deep_fert_df)
}



####Now make a sep df for treatments wetting agents ####NEEDS WORKS#####
function_wetter_df <- function(final_farm_df, wetter_year, wetter_cost){
  a <- distinct(final_farm_df, year, .keep_all = TRUE)
  b <- data_frame(year = as.numeric(wetter_year), #as numeric
                  cost = wetter_cost) #,
  #treatment = "rip_no_input")
  wetter_df <- left_join(a, b, "year")
  wetter_df <- select(wetter_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "wetter")
  wetter_df <- left_join(wetter_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  wetter_df <- wetter_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  wetter_df$year <- as.integer(wetter_df$year)
  wetter_df <- wetter_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
  
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(wetter_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  wetter_df <- left_join(wetter_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  wetter_df <- left_join(wetter_df, yld_resp_crop_rip, by = 'crop')
  wetter_df <- mutate(wetter_df, treatment = "wetter")
  wetter_df <- unite(wetter_df, ID,
                            c(year,treatment), remove = FALSE)
  #write.csv(wetter_df, "wetter_df.csv")
  return(wetter_df)
}


###Join the treatment df together with rbind
function_treatment_bind <- function(cost_rip_noinput_df, rip_shallow_organic_df,rip_shallow_fert_df, 
                                    rip_deep_organic_df, rip_deep_fert_df,wet_df ){
      treatment <- bind_rows(cost_rip_noinput_df, rip_shallow_organic_df, rip_shallow_fert_df, 
                             rip_deep_organic_df, rip_deep_fert_df,wet_df )
}
  #join the two df - Need a better way if I have multiple
#fix up na for clm before they are used in cals

function_final_treatment_farm <- function(final_farm_df, treatment_bind){
  a <- left_join(final_farm_df, treatment_bind, by = 'ID')
  b <- select(a, ID, year = year.x, crop = crop.x, treatment = treatment.x, cost, yld_resp_since_applied,
              yr_since_app, yld_resp_perct_crop, discount, current_yld, potential_yld, price)
  b <- replace_na(b,list(crop =0, cost=0, yld_resp_since_applied=0, 
             yr_since_app =0, yld_resp_perct_crop =0, discount =0, 
             current_yld =0, potential_yld =0, price =0))
  #write.csv(b, "final_treatment_farm.csv")
  return(b)
}


#### economic indicators ####
function_economic_indicators <- function(final_treatment_farm) {
  #work out the econmoic indicators
  
  economic_indicators_rip_no_input <- filter(final_treatment_farm, treatment == 'rip_no_inputs')
  economic_indicators_rip_no_input <-  economic_indicators_rip_no_input %>% 
  mutate(
    pres_value_fact = (1/(1+discount)^ year),
    benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
    cashflow_no_dis_ann = benefit - cost,
    cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
    cashflow_cum = cumsum(cashflow_no_dis_ann),
    cashflow_cum_disc = cumsum(cashflow_dis_ann),
    ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
    ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
    benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
    npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
  )
  
  economic_indicators_rip_shallow_organic <- filter(final_treatment_farm, treatment == 'rip_shallow_organic')
  economic_indicators_rip_shallow_organic <-  economic_indicators_rip_shallow_organic %>% 
    mutate(
      pres_value_fact = (1/(1+discount)^ year),
      benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
      cashflow_no_dis_ann = benefit - cost,
      cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
      cashflow_cum = cumsum(cashflow_no_dis_ann),
      cashflow_cum_disc = cumsum(cashflow_dis_ann),
      ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
      ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
      benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
      npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
    )
  
  economic_indicators_rip_shallow_fert <- filter(final_treatment_farm, treatment == 'rip_shallow_fert')
  economic_indicators_rip_shallow_fert <-  economic_indicators_rip_shallow_fert %>% 
    mutate(
      pres_value_fact = (1/(1+discount)^ year),
      benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
      cashflow_no_dis_ann = benefit - cost,
      cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
      cashflow_cum = cumsum(cashflow_no_dis_ann),
      cashflow_cum_disc = cumsum(cashflow_dis_ann),
      ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
      ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
      benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
      npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
    )
  
  economic_indicators_rip_deep_organic <- filter(final_treatment_farm, treatment == 'rip_deep_organic')
  economic_indicators_rip_deep_organic <-  economic_indicators_rip_deep_organic %>% 
    mutate(
      pres_value_fact = (1/(1+discount)^ year),
      benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
      cashflow_no_dis_ann = benefit - cost,
      cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
      cashflow_cum = cumsum(cashflow_no_dis_ann),
      cashflow_cum_disc = cumsum(cashflow_dis_ann),
      ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
      ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
      benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
      npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
    )
  
  economic_indicators_rip_deep_fert <- filter(final_treatment_farm, treatment == 'rip_deep_fert')
  economic_indicators_rip_deep_fert <-  economic_indicators_rip_deep_fert %>% 
    mutate(
      pres_value_fact = (1/(1+discount)^ year),
      benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
      cashflow_no_dis_ann = benefit - cost,
      cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
      cashflow_cum = cumsum(cashflow_no_dis_ann),
      cashflow_cum_disc = cumsum(cashflow_dis_ann),
      ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
      ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
      benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
      npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
    )
  economic_indicators_wetter <- filter(final_treatment_farm, treatment == 'wetter')
  economic_indicators_wetter <-  economic_indicators_wetter %>% 
    mutate(
      pres_value_fact = (1/(1+discount)^ year),
      benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
      cashflow_no_dis_ann = benefit - cost,
      cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
      cashflow_cum = cumsum(cashflow_no_dis_ann),
      cashflow_cum_disc = cumsum(cashflow_dis_ann),
      ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
      ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
      benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
      npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
    )
  economic_indicators <- bind_rows(economic_indicators_rip_no_input,
                                   economic_indicators_rip_shallow_organic,
                                   economic_indicators_rip_shallow_fert,
                                   economic_indicators_rip_deep_organic,
                                   economic_indicators_rip_deep_fert,
                                   economic_indicators_wetter)
  
#write.csv(economic_indicators, file = "economic_indicators.csv")
return(economic_indicators)
}

function_do_montecarlo_economic_indicators <- function(final_treatment_farm, num_simulation=NULL, dbn_name=NULL, decile_1=NULL, decile_9=NULL  ){
    #do monte-carlo on the economic_indicators,
    #distribution_properties is a named list with the minimal properties of attributes
    #distribution_properties = list(name="", location=0, shape=1, rate)
    
    #if(doDbg) browser()
    #do a single simulation 
    if(is.null(num_simulation) && is.null(dbn_name)){
        economic_indicators = function_economic_indicators(final_treatment_farm)
        return(economic_indicators)
    } 
    
    #init montecarlo params  
    if(is.null(num_simulation)) num_simulation <- 100
    if(is.null(dbn_name)) dbn_name <- "log-logistic"
    
    nominal_yield_wheat = 3.0 
    if(is.null(decile_1)) decile_1 <- nominal_yield_wheat*0.25 #pas067 change to realistic value
    if(is.null(decile_9)) decile_9 <- nominal_yield_wheat*2.0 #pas067 change to realistic value
    
    #do montecarlo, place the first element of the list without randomness
    mc_economic_indicators = list()
    mc_economic_indicators[[1]] = function_economic_indicators(final_treatment_farm)
    if(dbn_name == "log-logistic"){
        scale = sqrt(decile_9 * decile_1)
        shape = -2*log(3)/log(sqrt(decile_9*decile_1)/decile_9)
        # montecarloloop through the randomness, #pas067 negative values draw another, potentially correlate with other variable
        for(mc_idx in 2:num_simulation){
            mc_current_yld <-actuar::rllogis(length(final_treatment_farm$current_yld), shape=shape, scale = scale)
            final_treatment_farm$current_yld <- mc_current_yld
            mc_economic_indicators[[mc_idx]] = function_economic_indicators(final_treatment_farm)
        }
        
    }    
    #if(doDbg) browser()
    return(mc_economic_indicators)
    
} #function_do_montecarlo_economic_indicators

function_plot <- function(economic_indicators,  metric) {
    
  if(doDbg) browser()    
  economic_indicators$year <- round(economic_indicators$year, 0)
  
  economic_indicators$treatment <- factor(economic_indicators$treatment, c("wetter", "rip_no_inputs", "rip_shallow_organic",
                                                             "rip_shallow_fert", "rip_deep_organic",
                                                             "rip_deep_fert"))
  
  #m <- sym(metric)
  ggplot(economic_indicators, aes_(x = as.name("year"), y=as.name(metric) , colour= as.name("treatment")))+
    geom_line()+
    theme_classic()+
    theme(legend.position = "bottom")+
    scale_color_manual(name = "",
                       labels=c(wetter = "wetter", 
                                rip_no_inputs ="ripping with no inputs", 
                                rip_shallow_organic = "ripping with shallow organic inputs", 
                                rip_shallow_fert ="ripping with shallow fertiliser inputs", 
                                rip_deep_organic="ripping with deep organic inputs",
                                rip_deep_fert = "ripping with deep fertiliser inputs"),
                       values=c(wetter = "black", 
                                rip_no_inputs = "grey",
                                rip_shallow_organic = "light blue",
                                rip_shallow_fert ="dark blue", 
                                rip_deep_organic= "light green", 
                                rip_deep_fert = "dark green"))+
    xlim(1,5)+
    labs(x = "Years",
         y = "$")
  
}

function_plot_list_economic_indicators <- function(list_economic_indicators, metric){
    #pas067 plots economic_indicators of interest from a list of data.frames of economic indicators
    if(doDbg) browser() 
    economic_indicators_1 <- list_economic_indicators[[1]]
    
    if(FALSE){
    #from the list create a data.frame just containing the metric
    list_dfmetric <- lapply(list_economic_indicators, function(x) x%>% select(metric))
    list_dfmetric[[length(list_dfmetric)+1]] = economic_indicators_1["year"]
    df_slctd_metric <- bind_cols(list_dfmetric)
    dfs2plot <- reshape2::melt(df_slctd_metric, id.vars="year")
    } else{
        df <- data.frame(year = 1:10,
                         a = cumsum(rnorm(10)),
                         b = cumsum(rnorm(10)),
                         c = cumsum(rnorm(10)))
        dfs2plot <- melt(df ,  id.vars = 'year', variable.name = 'series')        
    }

    
    if(TRUE) browser()
    
    economic_indicators_1$year <- round(economic_indicators_1$year, 0)
    
    economic_indicators_1$treatment <- factor(economic_indicators_1$treatment, c("wetter", "rip_no_inputs", "rip_shallow_organic",
                                                                             "rip_shallow_fert", "rip_deep_organic",
                                                                             "rip_deep_fert"))
    
    #from the list create a data.frame just containing the metric
    is_many = TRUE
    if(is_many){
        ggplot(dfs2plot,
               aes(x=year, y=value)) +
            geom_line()
    } else{
        #m <- sym(metric)
        ggplot(economic_indicators_1, aes_(x = as.name("year"), y=as.name(metric) , colour= as.name("treatment")))+
            geom_line()+
            theme_classic()+
            theme(legend.position = "bottom")+
            scale_color_manual(name = "",
                               labels=c(wetter = "wetter", 
                                        rip_no_inputs ="ripping with no inputs", 
                                        rip_shallow_organic = "ripping with shallow organic inputs", 
                                        rip_shallow_fert ="ripping with shallow fertiliser inputs", 
                                        rip_deep_organic="ripping with deep organic inputs",
                                        rip_deep_fert = "ripping with deep fertiliser inputs"),
                               values=c(wetter = "black", 
                                        rip_no_inputs = "grey",
                                        rip_shallow_organic = "light blue",
                                        rip_shallow_fert ="dark blue", 
                                        rip_deep_organic= "light green", 
                                        rip_deep_fert = "dark green"))+
            xlim(1,5)+
            labs(x = "Years",
                 y = "$")
    }#is_many
    
}

function_metric_name <- function(metric){
  
    ifelse(metric == cashflow_no_dis_ann, "Undiscounted annual cash flow",
                    ifelse(test_name == cashflow_dis_ann, "discounted annual net cash flow",
                    ifelse(test_name == cashflow_cum_disc, "Cummulative discounted cash flow",
                    ifelse(test_name == ROI_cum_no_disc, "Cummulative ROI not discounted",                         
                    ifelse(test_name == ROI_cum_disc, "Cummulative ROI discounted",                         
                    ifelse(test_name == benefit_cost_ratio_disc, "Benefit:Cost Ratio (discounted)",                         
                    ifelse(test_name == npv, "Net Present Value",                        
                    ifelse(test_name == MIRR, "Modified Internal Rate Return"      
                    ))))))))      
 print(metric) 
}
