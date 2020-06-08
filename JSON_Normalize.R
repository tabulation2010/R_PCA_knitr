# Scaling

## Initialization
rm(list = ls())
library("jsonlite")
args=commandArgs(T)

# Load data
json_data <- fromJSON(txt = args[1])

input_df <- json_data$data
predictor_list <- json_data$nameList

normlized <- as.data.frame(scale(input_df[predictor_list], center = TRUE, scale = TRUE))

input_df[predictor_list] <- normlized[predictor_list]
json_data$data <- input_df
write_json(json_data, path = args[2])

# write.csv(normlized, file = "a.csv", row.names = F)
