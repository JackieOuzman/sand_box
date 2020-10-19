

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



rm(cost_table_test, yld_table_test, extra_cost_benefits_table_test)


function_economics_tb_sc1 <- function(cost_sc1,yld_sc1, extra_sc1, sc1, run_of_years ){


## replace all na with 0 value
cost_sc1[is.na(cost_sc1)] <- 0
yld_sc1[is.na(yld_sc1)] <- 0
extra_sc1[is.na(extra_sc1)] <- 0

#value of yield
yld_sc1 <- yld_sc1 %>% 
  mutate(yld_gain_value = (`yield (modified)` - `yield  (un modified)`) * price) %>% 
  mutate(scenario = paste0("scenario ", sc1)) %>% 
  select(scenario, year, yld_gain_value )

#add a clm that has type is it a cost or saving
extra_sc1 <- extra_sc1 %>% 
  mutate(type =  case_when(
    activity == "additional costs ($/ha)" ~ "cost",
    activity == "cost harvesting and handling extra grain $/t" ~ "cost",
    activity == "additional savings ($/ha)" ~ "benefit",
    TRUE ~ activity
  ))

extra_benefits_cost_sc1 <- extra_sc1 %>% 
  group_by(year, type ) %>% 
  summarise(value = sum(value, na.rm = TRUE)) %>% 
 ungroup()

extra_benefits_sc1 <-  extra_benefits_cost_sc1 %>% 
  filter(type == "benefit") %>% 
  mutate(scenario = paste0("scenario ", sc1)) %>% 
  select(scenario, year, value )

#Add the beneifits to the yld table
benefits_sc1 <- left_join(yld_sc1, extra_benefits_sc1) %>% 
  mutate(total_benefit = yld_gain_value + value) %>% 
  select(scenario  ,  year, total_benefit )



##### now for the costs
extra_cost_sc1 <-  extra_benefits_cost_sc1 %>% 
  filter(type == "cost") %>% 
  mutate(scenario = paste0("scenario ", sc1)) %>% 
  select(scenario, year, value ) %>% 
  rename(total_cost =value)



### inital costs
intial_cost_sc1 <- cost_sc1 %>% 
  summarise(total_cost = sum(price, na.rm = TRUE)) %>% 
  mutate(scenario = paste0("scenario ", sc1)) %>% 
  mutate(year = 0) %>% 
  select(scenario, year, total_cost )



total_cost_sc1 <- rbind(intial_cost_sc1, extra_cost_sc1)
economics_tbl_sc1 <- left_join(total_cost_sc1, benefits_sc1)
economics_tbl_sc1[is.na(economics_tbl_sc1)] <- 0
economics_tbl_sc1 <- economics_tbl_sc1 %>% 
  mutate(undiscounted_cash_flow = total_benefit - total_cost)

economics_tbl_sc1 <- economics_tbl_sc1[ 1: (run_of_years+1),]



return(economics_tbl_sc1)
}

### use the function
economics_tbl_sc1 <- function_economics_tb_sc1(cost_sc1,yld_sc1, extra_sc1, 1, 3)
economics_tbl_sc2 <- function_economics_tb_sc1(cost_sc1,yld_sc1, extra_sc1, 2, 3)

#### STOP and check it has worked


print(economics_tbl_sc1)
print(economics_tbl_sc2)

ggplot(data= economics_tbl_sc1, aes(x= year, y = undiscounted_cash_flow))+
#ggplot(data= economics_tbl_sc1_2, aes(x= year_numb, y = value, colour = scenario))+
  geom_line()+
  theme_bw()+
  xlab("Years after modification") + ylab("Undiscounted cash flow $/ha") +
  ggtitle("Scenarios")

