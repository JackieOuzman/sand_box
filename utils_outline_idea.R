##this just defines my function...


function_base_df1 <- function(crop_seq_zone1, discount){
  data.frame(year = 1:length(crop_seq_zone1),
             crop = crop_seq_zone1,
             discount = discount) 
}

