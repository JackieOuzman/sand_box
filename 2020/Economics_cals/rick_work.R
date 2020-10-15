

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)

# bring in the data table I want 
cost_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                        sheet = "DT_yld")
extra_cost_benefits_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                        sheet = "DT_extra_cost_benefits")


# filter the data table this will be the dynamic bit in app
cost_sc1 <- cost_table_test %>% filter(modification == "Ripping 40cm" &
                                                site == "Murlong")

yld_sc1 <- yld_table_test %>% filter(modification == "Ripping 40cm" &
                                                          site == "Murlong")
extra_sc1 <- extra_cost_benefits_table_test %>% filter(modification == "Ripping 40cm" &
                                                        site == "Murlong")





## Job 1 get the extra table into a wide format.

extra_sc1 <- extra_sc1 %>% 
  select(-grouping, -modification, -site) %>% 
  mutate(year = paste0("year ", year)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  relocate(c(comments,`data source`), .after = last_col()) 


# use selected data and make a table that looks like Ricks

#1. the cost table
cost_sc1_yr0 <-  sum(cost_sc1$price)
#extra cost and benefit table
cost_extra_sc1 <- extra_sc1[c(1,3),]
cost_annual_sc1 <- colSums(extra_sc1[sapply(extra_sc1, is.numeric)], na.rm = TRUE)

#extra cost and benefit table
extra_benefit_sc1 <- extra_sc1[2,]
benefit_annual_sc1 <- colSums(extra_benefit_sc1[sapply(extra_benefit_sc1, is.numeric)], na.rm = TRUE)





#make the df  this is wide?
activity <- c('cost_treatment','cost_annual','benefit_yld_unmod', 'benefit_yld_mod','benefit_yld_gain',
              'benefit_annual', 'benefit_crop','benefit_grain_price')
year0 <- as.numeric(c(cost_sc1_yr0, 0,      0, 0, 0, 0,0, 0))
year1 <- as.numeric(c(0, cost_annual_sc1[1],  yld_sc1[[1,6]], yld_sc1[[1,7]], (yld_sc1[[1,7]]- yld_sc1[[1,6]]),benefit_annual_sc1[1], yld_sc1[[1,5]], yld_sc1[[1,8]]))
year2 <- as.numeric(c(0, cost_annual_sc1[2],  yld_sc1[[2,6]], yld_sc1[[2,7]], (yld_sc1[[2,7]]- yld_sc1[[2,6]]),benefit_annual_sc1[2], yld_sc1[[2,5]], yld_sc1[[2,8]]))
year3 <- as.numeric(c(0, cost_annual_sc1[3],  yld_sc1[[3,6]], yld_sc1[[3,7]], (yld_sc1[[3,7]]- yld_sc1[[3,6]]),benefit_annual_sc1[3], yld_sc1[[3,5]], yld_sc1[[3,8]]))
year4 <- as.numeric(c(0, cost_annual_sc1[4],  yld_sc1[[4,6]], yld_sc1[[4,7]], (yld_sc1[[4,7]]- yld_sc1[[4,6]]),benefit_annual_sc1[4], yld_sc1[[4,5]], yld_sc1[[4,8]]))
year5 <- as.numeric(c(0, cost_annual_sc1[5],  yld_sc1[[5,6]], yld_sc1[[5,7]], (yld_sc1[[5,7]]- yld_sc1[[5,6]]),benefit_annual_sc1[5], yld_sc1[[5,5]], yld_sc1[[5,8]]))
df <- data.frame(activity, year0, year1, year2, year3, year4,year5)

rm("extra", "extra_sc1", "yld_sc1",  "extra_cost_sc1")


# do the undiscounted cash flow cals
#sum of costs
cost_rows <- df[c(1,2),2:7]
cost_sum <- apply(cost_rows, 2, sum) #the 2 is rows or clms

#sum of benefits
benefit_rows <- df[c(5,8,6), 2:7]
benefit_sum <- (benefit_rows[1, ] * benefit_rows[2, ])+benefit_rows[3, ]


undiscounted_cash_flow <- benefit_sum - cost_sum

sc1 <- pivot_longer(undiscounted_cash_flow, 
                    cols = c("year0",  "year1",    "year2",    "year3", "year4", "year5"),
                    names_to = "year")

sc1
sc1 <- sc1 %>% mutate(
  year_numb = case_when(
    year == "year0" ~ "0",
    year == "year1" ~ "1",
    year == "year2" ~ "2",
    year == "year3" ~ "3",
    year == "year4" ~ "4",
    year == "year5" ~ "5",
    TRUE ~year)) %>% 
  mutate( scenario = "Scenario 1")

sc1$year_numb <- as.numeric(sc1$year_numb)


rm("df", "undiscounted_cash_flow", "cost_rows", "cost_sc1", "extra_benefit_sc1")
######################################################################################################


# filter the data table this will be the dynamic bit in app
cost_sc2 <- cost_table_test %>% filter(modification == "Ripping 60cm" &
                                         site == "Murlong")

yld_sc2 <- yld_table_test %>% filter(modification == "Ripping 60cm" &
                                       site == "Murlong")
extra_sc2 <- extra_cost_benefits_table_test %>% filter(modification == "Ripping 60cm" &
                                                         site == "Murlong")


# remove the file I dont need
rm("cost_table_test", "yld_table_test", "extra_cost_benefits_table_test")


## Job 1 get the extra table into a wide format.

extra_sc2 <- extra_sc2 %>% 
  select(-grouping, -modification, -site) %>% 
  mutate(year = paste0("year ", year)) %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  relocate(c(comments,`data source`), .after = last_col()) 


# use selected data and make a table that looks like Ricks

#1. the cost table
cost_sc2_yr0 <-  sum(cost_sc2$price)
#extra cost and benefit table
cost_extra_sc2 <- extra_sc2[c(1,3),]
cost_annual_sc2 <- colSums(extra_sc2[sapply(extra_sc2, is.numeric)], na.rm = TRUE)

#extra cost and benefit table
extra_benefit_sc2 <- extra_sc2[2,]
benefit_annual_sc2 <- colSums(extra_benefit_sc2[sapply(extra_benefit_sc2, is.numeric)], na.rm = TRUE)


#make the df  this is wide?
activity <- c('cost_treatment','cost_annual','benefit_yld_unmod', 'benefit_yld_mod','benefit_yld_gain',
              'benefit_annual', 'benefit_crop','benefit_grain_price')
year0 <- as.numeric(c(cost_sc2_yr0, 0,      0, 0, 0, 0,0, 0))
year1 <- as.numeric(c(0, cost_annual_sc2[1],  yld_sc2[[1,6]], yld_sc2[[1,7]], (yld_sc2[[1,7]]- yld_sc2[[1,6]]),benefit_annual_sc2[1], yld_sc2[[1,5]], yld_sc2[[1,8]]))
year2 <- as.numeric(c(0, cost_annual_sc2[2],  yld_sc2[[2,6]], yld_sc2[[2,7]], (yld_sc2[[2,7]]- yld_sc2[[2,6]]),benefit_annual_sc2[2], yld_sc2[[2,5]], yld_sc2[[2,8]]))
year3 <- as.numeric(c(0, cost_annual_sc2[3],  yld_sc2[[3,6]], yld_sc2[[3,7]], (yld_sc2[[3,7]]- yld_sc2[[3,6]]),benefit_annual_sc2[3], yld_sc2[[3,5]], yld_sc2[[3,8]]))
year4 <- as.numeric(c(0, cost_annual_sc2[4],  yld_sc2[[4,6]], yld_sc2[[4,7]], (yld_sc2[[4,7]]- yld_sc2[[4,6]]),benefit_annual_sc2[4], yld_sc2[[4,5]], yld_sc2[[4,8]]))
year5 <- as.numeric(c(0, cost_annual_sc2[5],  yld_sc2[[5,6]], yld_sc2[[5,7]], (yld_sc2[[5,7]]- yld_sc2[[5,6]]),benefit_annual_sc2[5], yld_sc2[[5,5]], yld_sc2[[5,8]]))
df <- data.frame(activity, year0, year1, year2, year3, year4,year5)

rm("extra", "extra_sc2", "yld_sc2", "yld_table_selection_test_sc2", "extra_cost_sc2")
# do the undiscounted cash flow cals
#sum of costs
cost_rows <- df[c(1,2),2:7]
cost_sum <- apply(cost_rows, 2, sum) #the 2 is rows or clms

#sum of benefits
benefit_rows <- df[c(5,8,6), 2:7]
benefit_sum <- (benefit_rows[1, ] * benefit_rows[2, ])+benefit_rows[3, ]


undiscounted_cash_flow <- benefit_sum - cost_sum
undiscounted_cash_flow

sc2 <- pivot_longer(undiscounted_cash_flow, 
                    cols = c("year0",  "year1",    "year2",    "year3", "year4", "year5"),
                    names_to = "year")

sc2
sc2 <- sc2 %>% mutate(
  year_numb = case_when(
    year == "year0" ~ "0",
    year == "year1" ~ "1",
    year == "year2" ~ "2",
    year == "year3" ~ "3",
    year == "year4" ~ "4",
    year == "year5" ~ "5",
    TRUE ~year)) %>% 
  mutate( scenario = "Scenario 2")

sc2$year_numb <- as.numeric(sc2$year_numb)



rm(list= ls()[!(ls() %in% c('sc1','sc2'))])


##############################################################################################

#join sc1 and sc2
sc1_2 <- bind_rows(sc1, sc2)
print(sc1_2)
getwd()
write.csv(sc1_2, "C:/Users/ouz001/working_from_home/ripper/2020/sc1_2.csv")

ggplot(data= sc1_2, aes(x= year_numb, y = value, colour = scenario))+
  geom_line()+
  theme_bw()+
  xlab("Years after modification") + ylab("Undiscounted cash flow $/ha") +
  ggtitle("Scenarios")
