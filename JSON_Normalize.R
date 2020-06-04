# Scaling

## Initialization
rm(list = ls())
library("jsonlite")

# Load data
json_data <- fromJSON(txt = "./Data/data_norm.json")

input_df <- json_data$data
predictor_list <- json_data$nameList

normlized <- as.data.frame(scale(input_df[predictor_list], center = TRUE, scale = TRUE))

write.csv(normlized, file = "./normalized_data_norm.csv", row.names = F)