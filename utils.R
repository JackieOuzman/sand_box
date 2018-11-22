#this function creates a url to download the rainfall data
#it requires a sand site name passed a "" and returns a dataframe
library(XML)
library(RCurl)
library(readr)

getSiloMet_jax <- function(name = NULL) {
  stationID <- ifelse(name == 'Waikerie', 24018,
               ifelse(name == 'Carwarp', 76005,
               ifelse(name == 'Ouyen', 76047,
               ifelse(name == 'Karoonda', 25006,
               ifelse(name == 'Murlong', 18046,
               ifelse(name == 'Yenda', 75079,
               ifelse(name == 'Lameroo', 25509,
               ifelse(name == 'Bute', 21012,
               ifelse(name == 'Brimpton Lake', 18005,
               ifelse(name == 'Cadgee', 26099,18005))))))))))
  
  #check my apiKey it is Kvsj6LwGthiRRUoxGazEDLikHvDxh5kOJDvbRZp4
  startDate <- paste('19600101',sep='')
  finishDate <- paste('20171231',sep='')
  
  
  siloUrl <- 'https://siloapi.longpaddock.qld.gov.au/pointdata'
  siloUrl <- paste(siloUrl, '?apikey=','Kvsj6LwGthiRRUoxGazEDLikHvDxh5kOJDvbRZp4', sep='')
  siloUrl <- paste(siloUrl,'&station=',sprintf('%05d', stationID), sep='')
  siloUrl <- paste(siloUrl,'&start=', startDate, sep='')
  siloUrl <- paste(siloUrl,'&finish=', finishDate, sep='')
  siloUrl <- paste(siloUrl,'&format=CSV', sep='')
  siloUrl <- paste(siloUrl,'&variables=daily_rain,max_temp,min_temp', sep='')
  
  print(siloUrl)
  df <- read_csv(siloUrl)
  return(df)
}

test_met <- getSiloMet_jax("Karoonda")

#2nd function to use.
library(dplyr)
library(lubridate)
#This cal the available water based on downloaded met file
#input is a met df with daily rainfall data and output is ....

cal_water_avail <- function(met_file) {
  rainfall_1 <- select(met_file, date, daily_rain, station)
  #rainfall_1 <- select(test_met, date, daily_rain, station)
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
  
  water_aval <- mutate(water_aval,FS_yld_pot_wheat = (((Rain_GS_summer - 60)*22)*1.12)/1000) 
  
  glimpse(water_aval)
  
  return(water_aval)
}

#function to cal the DECILE YEARS for a met file
#take as met file that is processed to have aviliable water clm
#and output a table of deciles
#library(dplyr)
decile <- function(water_aval) {
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
  
  #count(yield_pot1,Decile)
  #str(yield_pot1)
  
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
  
  ###mess about with tables####
  
  return(Analogue_yrs)
}
#test_decile <- decile(water_aval)
