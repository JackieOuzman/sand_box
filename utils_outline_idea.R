##this just defines my function...

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



#added treatments to get it like Alex example
#orginal
#function_base_df1 <- function(crop_seq_zone1, discount){
#  x <- data.frame(year = 1:length(crop_seq_zone1),
#                  crop = crop_seq_zone1,
#                  discount = discount)
#  y <- pre <- data.frame(year = 0)
#  bind_rows(y, x)
#}


#this is working but the year 0 has no treatment assigned
function_base_df1 <- function(mangement_options,crop_seq_zone1, discount){
  x <- data.frame(treatments = mangement_options,
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
  left_join(join_potential_df, flip_df_price, by = 'crop')
}

#Now make a sep df for treatments with crops, year costs and yield response
#having trouble with assigning cost to year zero but not the yield response
#need to sort this out
function_treatments_df <- function(join_price_df, year_for_ripping, costs_ripping){
  #a <- data_frame(select(join_price_df, year, crop))
  a <- join_price_df
  b <- data_frame(year = as.numeric(year_for_ripping), #as numeric
                  cost = costs_ripping)
  cost_df <- left_join(a, b, "year")
  cost_df <- select(cost_df, year, crop, cost)
  
  #bring in a file with the yield response over the 10 years
  yld_resp_crop_treat <- read.csv("yld_response.csv")
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
    select(year, cost, treatment, yld_reponse, crop, yr_since_app)
  #making temp file for a join which has a dummy yr_since_app clm
  treat <- select(cost_df, year, cost, yld_reponse)
  treat <- mutate(treat,yr_since_app = year )
  cost_df <- left_join(cost_df, treat, by = 'yr_since_app') %>% 
    select(year = year.x, treatment, crop, cost = cost.x, yld_resp_since_applied
           = yld_reponse.y, yr_since_app)
  #bring in another yield response file relating everything to crop type
  yld_resp_crop_rip <- read.csv("yld_response_by_crop_ripping.csv")
  cost_df <- left_join(cost_df, yld_resp_crop_rip, by = 'crop')
  
}

#join the two df - Need a better way if I have multiple
#fix up na for clm before they are used in cals

function_final_df <- function(join_price_df, treatments_df){
  a <- left_join(join_price_df, treatments_df, by = 'year')
  b <- select(a, year, crop = crop.x, cost, yld_resp_since_applied,
              yr_since_app, yld_resp_perct_crop, discount, current_yld, potential_yld, price)
  b <- replace_na(b,list(crop =0, cost=0, yld_resp_since_applied=0, 
             yr_since_app =0, yld_resp_perct_crop =0, discount =0, 
             current_yld =0, potential_yld =0, price =0))
}

#fix up na for clm before they are used in cals

#function_final_df_na_rm <- function(final_df){
#  replace_na(final_df,list(crop =0), cost=0, yld_resp_since_applied=0, 
#             yr_since_app =0, yld_resp_perct_crop =0, discount =0, 
#             current_yld =0, potential_yld =0, price =0)
#}

function_economic_indicators <- function(final_df) {
  #work out the econmoic indicators
  final_df <- final_df %>% 
  mutate(
    pres_value_fact = (1/(1+discount)^ year),
    benefit = ((current_yld*(yld_resp_since_applied / 100)* yld_resp_perct_crop) * price), 
    cashflow_no_dis_ann = benefit - cost,
    cashflow_dis_ann = ((benefit*pres_value_fact) - (cost*pres_value_fact)), 
    cashflow_cum_disc = cumsum(cashflow_dis_ann),
    ROI_cum_no_disc = (cumsum(benefit) - cumsum(cost))/ cumsum(cost), 
    ROI_cum_disc = (cumsum(benefit*pres_value_fact) - cumsum(cost*pres_value_fact))/ cumsum(cost*pres_value_fact), 
    benefit_cost_ratio_disc = (sum(benefit*pres_value_fact) / sum(cost*pres_value_fact)), 
    npv = (sum(benefit*pres_value_fact) - sum(cost*pres_value_fact))
  )
  
write.csv(final_df, file = "check_on_outputs.csv")
return(final_df)
}


function_plot <- function(economic_indicators) {
  ggplot(economic_indicators, aes(year, cashflow_no_dis_ann))+
    geom_line()
}
