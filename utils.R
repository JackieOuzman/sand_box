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
  glimpse(water_aval)
  
  return(water_aval)
}

#check <- cal_water_avail(test_met)


