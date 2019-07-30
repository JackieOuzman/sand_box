##this just defines my function...
#this get the downlaoded met files they are in a folder called met
#I was thinking this would be quicker than using the uRL function I had before
#But it is just as slow:(

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
###########################################################################################################
######                                      END OF MET WORK                               ################
###########################################################################################################




###########################################################################################################
####                               MAKING A DF FOR THE FARM                                ###############
###########################################################################################################



function_base_df1 <- function(mangement_options){
  x <- data.frame(treatment = mangement_options,
                  #year = 1:length(crop_seq),
                  year = 1:10,
                  crop = "wheat")
                  #discount = discount)
  y <- pre <- data.frame(year = 0)
  bind_rows(y, x)
  #write_csv(step1, "Step1_yr_crop_disc_mang.csv")
}


#step 1
function_making_df_current <- function(P5, P50, P90, a, base_df1 ){
  step2 <- data.frame( Wheat_P5 = P5,
                       Wheat_P50 = P50,
                       Wheat_P90 = P90,
                       Wheat_price = a)
  step3 <- mutate(step2, crop = "wheat")
  step4 <- left_join(base_df1, step3, by = "crop") 
  
  step5 <- fill(step4, treatment,.direction = c("up")) #this fills in missing values
  step6 <- unite(step5, ID,
                         c(year,treatment), remove = FALSE) #this is making an ID clm
  #write_csv(step2, "step2_wheatP_5_50_90.csv")
  #write_csv(step4, "step4_df_yld.csv")
  #write_csv(step5, "step5_df_yld.csv")
  write_csv(step6, "step6_df_yld.csv") #happy with
}




###################################################################################### 
###########                 DF FOR TREATMENTS          ##############################
###################################################################################### 
###########                   shallow_input            ############################## 
###################################################################################### 

##New plan is to make treatment df for each of the treatments we have ###
## Then join to the final_farm_df the missing treatments wont join###

#Now make a sep df for treatments Ripping with shallow inputs first
function_rip_shallow_input_df <- function(making_df_current, year_for_ripping, cost_shallow){
  a <-  distinct(making_df_current, year, .keep_all = TRUE)
  b <-  data_frame(year = as.numeric(year_for_ripping), #as numeric
                  cost = cost_shallow) 
                  
  cost_rip_shallow_input_df <- left_join(a, b, "year")
  cost_rip_shallow_input_df <- select(cost_rip_shallow_input_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_shallow_input")
  cost_rip_shallow_input_df <- left_join(cost_rip_shallow_input_df,yld_resp_crop_treat, "year")
  #step3 cal to modify the yield response reflecting when the treatment was applied
  cost_rip_shallow_input_df <- cost_rip_shallow_input_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  cost_rip_shallow_input_df$year <- as.integer(cost_rip_shallow_input_df$year)
  cost_rip_shallow_input_df <- cost_rip_shallow_input_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)
 #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(cost_rip_shallow_input_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  cost_rip_shallow_input_df <- left_join(cost_rip_shallow_input_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  cost_rip_shallow_input_df <- left_join(cost_rip_shallow_input_df, yld_resp_crop_rip, by = 'crop')
  cost_rip_shallow_input_df <- mutate(cost_rip_shallow_input_df, treatment = "rip_shallow_input")
  cost_rip_shallow_input_df <- unite(cost_rip_shallow_input_df, ID,
                         c(year,treatment), remove = FALSE)
  
  write.csv(cost_rip_shallow_input_df, "step1_treatment_shallow.csv")
  return(cost_rip_shallow_input_df)
 
  }
######################################################################################
###########                   deep_input            ############################## 
###################################################################################### 

#Now make a sep df for treatments Ripping with deep inputs first
function_rip_deep_input_df <- function(making_df_current, rip_deep_year, rip_deep_cost){
  a <-  distinct(making_df_current, year, .keep_all = TRUE)
  b <-  data_frame(year = as.numeric(rip_deep_year), #as numeric
                   cost = rip_deep_cost) 
  
  cost_rip_deep_input_df <- left_join(a, b, "year")
  cost_rip_deep_input_df_test <- select(cost_rip_deep_input_df, year, crop, cost)
  

  #######bring in a file with the yield response over the 10 years #######
  
  yld_resp_crop_treat <- read.csv("yld_response.csv")

  yld_resp_crop_treat <- filter(yld_resp_crop_treat, treatment == "rip_deep_input")
  cost_rip_deep_input_df <- left_join(cost_rip_deep_input_df,yld_resp_crop_treat, "year")
  #######step3 cal to modify the yield response reflecting when the treatment was applied #######
  cost_rip_deep_input_df <- cost_rip_deep_input_df %>% 
    mutate(code = case_when(cost > 0 ~ 1,
                            cost == 0 ~ 0)) 
  cost_rip_deep_input_df$year <- as.integer(cost_rip_deep_input_df$year)
  cost_rip_deep_input_df <- cost_rip_deep_input_df %>%
    mutate(
      code = as.logical(code),
      last_event = if_else(code, true = year, false = NA_integer_)) %>%
    fill(last_event) %>%
    mutate(yr_since_app = (year - last_event)) %>% 
    select(year, cost, yld_reponse, crop, yr_since_app)


 
 ######          making temp file for a join which has a dummy yr_since_app clm   ######          
  treat <- select(cost_rip_deep_input_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  cost_rip_deep_input_df <- left_join(cost_rip_deep_input_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #######          bring in another yield response file relating everything to crop type   ######          
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  cost_rip_deep_input_df <- left_join(cost_rip_deep_input_df, yld_resp_crop_rip, by = 'crop')
  cost_rip_deep_input_df <- mutate(cost_rip_deep_input_df, treatment = "rip_deep_input")
  cost_rip_deep_input_df <- unite(cost_rip_deep_input_df, ID,
                                     c(year,treatment), remove = FALSE)
  
  write.csv(cost_rip_deep_input_df, "step1_treatment_deep.csv")
  return(cost_rip_deep_input_df)
}


####################################################################################################
##############                Join the treatment df together with rbind            ############## 
##################################################################################################                

function_treatment_bind <- function(cost_rip_deep_input_df, cost_rip_shallow_input_df ){
      treatment <- bind_rows(cost_rip_deep_input_df, cost_rip_shallow_input_df )
      treatment <- select(treatment, 
             ID, cost, yld_resp_since_applied, yr_since_app,
             yld_resp_perct_crop )
      write.csv(treatment, "step1_2_treament.csv")
      return(treatment)
}

####################################################################################################
##############                Join the treatment df with main_df            ############## 
################################################################################################## 
function_final_treatment_farm <- function(making_df_current, treatment_bind){
  
  a <- left_join(making_df_current, treatment_bind, by = 'ID')
  
  
  b <- replace_na(a,list(crop =0, cost=0, yld_resp_since_applied=0, 
                         yr_since_app =0, yld_resp_perct_crop =0,  
                         current_yld =0, potential_yld =0, price =0))
  write.csv(b, "final_treatment_farm.csv")
  return(b)
}


####################################################################################################
##############                economics                                              ############## 
################################################################################################## 


function_economic_indicators <- function(final_treatment_farm, production_area, N_applied, cost_N, insurance, levies,
                                         freight, variable_cost) {
  #work out the econmoic indicators
  
  economic_indicators_rip_shallow_input <- filter(final_treatment_farm, treatment == 'rip_shallow_input')
  economic_indicators_rip_shallow_input <-  economic_indicators_rip_shallow_input %>% 
  mutate(
    m_c_yld = Wheat_P50, #check this out with Ric
    new_yld = (m_c_yld * (yld_resp_since_applied / 100)* yld_resp_perct_crop) + m_c_yld,
    revenue = (new_yld * production_area) * Wheat_price, # need to make new UI input for 400ha as area of wheat production
    N_cost = (N_applied * (cost_N /100) * production_area), #rate of N kg/ha * cost of N $/t /100* area of production)
    freight_cost = freight* (production_area * new_yld), # cost of freight $17, area of production
    Insurance_levies = revenue * ((insurance + levies)/100), # insurance and levies
    variable_cost_cal = (variable_cost * production_area), # sum of varaible cost * area
    cost_treatmnet = (cost * production_area), #cost of treatment $/ha * area,
    total_direct_expenses = N_cost + freight_cost +Insurance_levies + variable_cost_cal + cost_treatmnet,
    gross_margin = revenue - total_direct_expenses
  )
  
  economic_indicators_rip_deep_input <- filter(final_treatment_farm, treatment == 'rip_deep_input')
  economic_indicators_rip_deep_input <-  economic_indicators_rip_deep_input %>% 
    mutate(
      m_c_yld = Wheat_P50, #check this out with Ric
      new_yld = (m_c_yld * (yld_resp_since_applied / 100)* yld_resp_perct_crop) + m_c_yld,
      revenue = (new_yld * production_area) * Wheat_price, # need to make new UI input for 400ha as area of wheat production
      N_cost = (N_applied * (cost_N /100) * production_area), #rate of N kg/ha * cost of N $/t /100* area of production)
      freight_cost = freight *(production_area * new_yld), # cost of freight $17, area of production
      Insurance_levies = revenue * ((insurance +levies)/100), # insurance and levies
      variable_cost_cal = (variable_cost * production_area), # sum of varaible cost * area
      cost_treatmnet = (cost * production_area), #cost of treatment $/ha * area,
      total_direct_expenses = N_cost + freight_cost +Insurance_levies + variable_cost_cal + cost_treatmnet,
      gross_margin = revenue - total_direct_expenses
    )
  
  economic_indicators <- bind_rows(economic_indicators_rip_shallow_input,
                                   economic_indicators_rip_deep_input)
  
write.csv(economic_indicators, file = "economic_indicators.csv")
return(economic_indicators)
}

###################################################################################
###############  mc sim needs some changes #######################################
#################################################################################

function_do_montecarlo_economic_indicators <- function(final_treatment_farm, 
                                                       num_simulation=NULL, 
                                                       dbn_name=NULL, decile_1=NULL, decile_9=NULL  ){
  #do monte-carlo on the economic_indicators,
  #distribution_properties is a named list with the minimal properties of attributes
  #distribution_properties = list(name="", location=0, shape=1, rate)
  #log-logistic" actuar::rllogis
  
  ###  JAX   #### REMOVED because I don't understand how this works if(doDbg) browser()
  #do a single simulation 
  if(is.null(num_simulation) && is.null(dbn_name)){
    economic_indicators = function_economic_indicators(final_treatment_farm, production_area, N_applied, cost_N, insurance, levies,
                                                       freight, variable_cost)
    return(economic_indicators)
  } 
  
  #init montecarlo params  
  if(is.null(num_simulation)) num_simulation <- 200
  if(is.null(dbn_name)) dbn_name <- "log-logistic"
  
  nominal_yield_wheat = 3.0 
  if(is.null(decile_1)) decile_1 <- nominal_yield_wheat*0.25 #pas067 change to realistic value
  if(is.null(decile_9)) decile_9 <- nominal_yield_wheat*2.0 #pas067 change to realistic value
  
  #do montecarlo, place the first element of the list without randomness
  mc_economic_indicators = list()
  mc_economic_indicators[[1]] = function_economic_indicators(final_treatment_farm, production_area, N_applied, cost_N, insurance, levies,
                                                             freight, variable_cost)
  if(dbn_name == "log-logistic"){
    scale = sqrt(decile_9 * decile_1) #TODO: check derivation, pas067 this is good for actua::loglogistic
    shape = -2*log(3)/log(sqrt(decile_9*decile_1)/decile_9) #pas067, this is good for actua::loglogisitc
    if(doDbg) browser()
    mc_idx <- 2
    while(mc_idx<=num_simulation){
      #generate random final final_treatment_farm$current_yld
      mc_current_yld <-actuar::rllogis(length(final_treatment_farm$Wheat_P50), shape=shape, scale = scale) #distribtuion
      #remove this current_yld when negative
      any_neg <- any(mc_Wheat_P50 < 0)
      if(any_neg) next() #go to while when negative
      final_treatment_farm$Wheat_P50 <- mc_Wheat_P50
      mc_economic_indicators[[mc_idx]] = function_economic_indicators(final_treatment_farm)
      mc_idx = mc_idx + 1
    }
  }    
  #### JAX removed because I don't understand how this works  if(doDbg) browser()
  return(mc_economic_indicators)
  
} #function_do_montecarlo_economic_indicators



#############################################################################################################
################    This is my old plot of results     ####################################################
###########################################################################################################

function_plot <- function(economic_indicators) {
  economic_indicators$year <- round(economic_indicators$year, 0)
  
  economic_indicators$treatment <- factor(economic_indicators$treatment, c("rip_shallow_input", "rip_deep_input"))
  
  #m <- sym(metric)
  ggplot(economic_indicators, aes_(x = as.name("year"),  y= as.name("gross_margin") , colour= as.name("treatment")))+
    geom_line(size=3)+
    theme_classic()+
    theme(legend.position = "bottom")+
    scale_color_manual(name = "",
                       labels=c(rip_shallow_input = "rip shallow with input", 
                                rip_deep_input ="rip deep with input"),
                       values=c(rip_shallow_input = "black", 
                                rip_deep_input = "grey"
                                ))+
    xlim(1,5)+
    scale_y_continuous(labels = scales::dollar_format())+
    labs(x = "Years",
         y = "GM")
  
}

#####################################################################################
########Plot for mc needs some changes #############
#####################################################################################
function_plot_list_economic_indicators <- function(list_economic_indicators
                                                   #, metric
                                                   ){
  #pas067 plots economic_indicators of interest from a list of data.frames of economic indicators
  #if(doDbg) browser() 
  economic_indicators_1 <- list_economic_indicators[[1]]
  
  #####from the list create a data.frame just containing the metric
  #list_dfmetric <- lapply(list_economic_indicators, function(x) x%>% select(metric))
  #list_dfmetric[[length(list_dfmetric)+1]] = economic_indicators_1["year"]
  #df_slctd_metric <- bind_cols(list_dfmetric)
  #vrbl_names = names(df_slctd_metric)
  #vrbl_names = vrbl_names[1:length(vrbl_names)-1]
  ######calculate the quartles of df_slctd_metric
  #df_names <- names(df_slctd_metric)
  #df_no_year <- df_slctd_metric[, df_names != "year"]
  #mtx_slctd_metric <- as.matrix(df_no_year)
  #mtx_row_ntiles <- rowQuantiles(mtx_slctd_metric, probs=seq(0,1, 0.25))
  #df_rowDeciles <- as.data.frame(mtx_row_ntiles)
  #df_year_rowDeciles <- cbind(select(df_slctd_metric, "year"), df_rowDeciles)
  
  #####formats for plotting many lines 
  #dfs2plot <- reshape2::melt(df_slctd_metric, id.vars="year", measure.vars = vrbl_names)
  #dcl2plot<- reshape2::melt(df_year_rowDeciles, id.vars="year")
  economic_indicators_1$year <- round(economic_indicators_1$year, 0)
  
  economic_indicators$treatment <- factor(economic_indicators$treatment, c("rip_shallow_input", "rip_deep_input"))
  
  #economic_indicators_1$treatment <- factor(economic_indicators_1$treatment, c("wetter", "rip_no_inputs", "rip_shallow_organic",
  #                                                                             "rip_shallow_fert", "rip_deep_organic",
  #                                                                             "rip_deep_fert"))
  
  #from the list create a data.frame just containing the metric
  is_many = TRUE
  if(is_many){
    ggplot(dcl2plot,
           aes(x=year, y=value, colour=variable)) +
      geom_line()
  } else{
    #m <- sym(metric)
    ggplot(economic_indicators_1, aes_(x = as.name("year"),  y= as.name("gross_margin") , colour= as.name("treatment")))+
      geom_line(size=3)+
      theme_classic()+
      theme(legend.position = "bottom")+
      scale_color_manual(name = "",
                         labels=c(rip_shallow_input = "rip shallow with input", 
                                  rip_deep_input ="rip deep with input"),
                         values=c(rip_shallow_input = "black", 
                                  rip_deep_input = "grey"
                         ))+
      xlim(1,5)+
      scale_y_continuous(labels = scales::dollar_format())+
      labs(x = "Years",
           y = "GM")
  }#is_many
  
}
