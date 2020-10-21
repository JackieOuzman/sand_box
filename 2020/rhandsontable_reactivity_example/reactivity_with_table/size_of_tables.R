cost_table <- read_excel("C:/Users/ouz001/working_from_home/ripper/2020/malcom_framework.xlsx", 
                         sheet = "DT_cost")

unique(cost_table$modification)
unique(cost_table$site)
#pretty sure they are all the same 4 rows

temp_cost <- filter(cost_table, modification == "Ploughing" &
         site == "Karoonda") %>% 
  select(activity , price, comments, `data source`)
#cost_table_selection[1:4,4:7]

print(temp_cost)
