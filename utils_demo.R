##this just defines my function...
function_df_2 <- function(treatment,treatment_2){
  data.frame(treatment = treatment,
             year = 1:length(treatment_2),
             crop_Type = treatment_2,
             value = rnorm(length(treatment_2), 50))
}

function_df_1 <- function(treatment,crop_seq_zone1){
  data.frame(treatment = treatment,
             year = 1:length(crop_seq_zone1),
             crop_Type = crop_seq_zone1,
             value = rnorm(length(crop_seq_zone1), 50))
}