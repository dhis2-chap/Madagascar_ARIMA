
library(dplyr) 
library(lubridate)

CHAP_df <- read.csv("input/training_data.csv")
#CHAP_df has the desired column names used internally by CHAP

original_df <- read.csv("input/malaria-u5-modelData.csv")
useful_df <- original_df[, c("csb", "date", "n_palu", "rain_mm", "temp_c")] #keeps the relevant columns

useful_df <- mutate(useful_df, date = as.Date(date))
colnames(useful_df) <- c("location", "time_period", "disease_cases", "rainfall", "mean_temperature")

useful_df <- mutate(useful_df, net_time = case_when(
  time_period<as.Date("2018-08-01") ~ as.numeric(time_period - as.Date("2015-08-01")+1)/365,
  time_period<as.Date("2021-08-01") ~ as.numeric(time_period - as.Date("2018-08-01")+1)/365,
  #to update when nets distribution happens
  time_period<as.Date("2025-08-01") ~ as.numeric(time_period - as.Date("2021-08-01")+1)/365
))

#split into training and future data, removed data before july 2016 as in the original code
train_df <- filter(useful_df, time_period > as.Date("2016-06-01") & time_period < as.Date("2023-01-01"))
test_df <- filter(useful_df, time_period >= as.Date("2023-01-01") & time_period < as.Date("2024-01-01"))
test_df <- subset(test_df, select= -disease_cases)

#Save them in the input folder as csv files
write.csv(train_df, "input/trainData.csv", row.names = FALSE)
write.csv(test_df, "input/futureClimateData.csv", row.names = FALSE)

