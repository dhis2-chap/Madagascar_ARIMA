source("train.R")
source("predict.R")

train_chap("input/trainData.csv", "output/model.bin")
predict_chap("output/model.bin", "input/trainData.csv", "input/futureClimateData.csv", "output/predictions.csv")

train_chap("input/training_data.csv", "output/model.bin")
predict_chap("output/model.bin", "input/historic_data.csv", "input/future_data.csv", "output/predictions_CHAP.csv")
