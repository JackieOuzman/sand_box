

library(dplyr)
library(ggplot2)
library(readxl)



cost_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")
yld_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                        sheet = "DT_yld")
extra_cost_benefits_table_test <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                                        sheet = "DT_extra_cost_benefits")


cost_table_selection_test <- cost_table_test %>% filter(modification == "Ripping 40cm" &
                                                site == "Murlong")

yld_table_selection_test <- yld_table_test %>% filter(modification == "Ripping 40cm" &
                                                          site == "Murlong")
extra_cost_benefits_selection_test <- extra_cost_benefits_table_test %>% filter(modification == "Ripping 40cm" &
                                                        site == "Murlong")
