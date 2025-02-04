
source("train.R")
source("predict.R")

train_chap("input/trainData.csv", "output/model.bin")
predict_chap("output/model.bin", "input/trainData.csv", "input/futureClimateData.csv", "output/predictions.csv")

#For testing with the CHAP-data locally
#train_chap("input/training_data.csv", "output/model.bin")
#predict_chap("output/model.bin", "input/historic_data.csv", "input/future_data.csv", "output/predictions_CHAP.csv")

#data wrangling 
# library(dplyr)
# df <- read.csv("input/futureData.csv")
# df <- mutate(df, time_period = substr(time_period, 1, 7))
#df$population <- rep(1, nrow(df))
#write.csv(df, file = "input/trainData.csv", row.names = FALSE)

