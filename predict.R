source("utils.R")

library(dplyr)
library(fable)
library(tsibble)
library(lubridate)
library(distributional) #to extract info from dist objects 

predict_chap <- function(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn) {
  future_per_location <- get_df_per_location(future_climatedata_fn)
  historic_per_location <- get_df_per_location(historic_data_fn)
  models <- readRDS(model_fn)  # Assumes the model was saved using saveRDS
  first_location <- TRUE
  
  for (location in names(future_per_location)){
    df <- future_per_location[[location]]
    historic_df <- historic_per_location[[location]]
    model <- models[[location]]
    
    df <- mutate(df, date = yearmonth(date)) #so tsibble understands it is monthly data, fails with exact date
    df <- create_lagged_feature(df, "rain_mm", 3, include_all = FALSE)$df
    df <- create_lagged_feature(df, "temp_c", 3, include_all = FALSE)$df
    df <- fill_top_rows_from_historic_last_rows("rain_mm", 3, df, historic_df)
    df <- fill_top_rows_from_historic_last_rows("temp_c", 3, df, historic_df)
    
    df_tsibble_new <- as_tsibble(df, index = date)
    
    predicted_dists <- forecast(model, new_data = df_tsibble_new)
    
    preds <- data.frame(matrix(ncol = 100, nrow = nrow(df_tsibble_new)))
    
    colnames(preds) <- paste("sample", 1:100, sep = "_")
    
    for(i in 1:nrow(df_tsibble_new)){
      dist <- predicted_dists[i, "n_palu"]$n_palu
      preds[i,] <- rnorm(100, mean = mean(dist), sd = sqrt(variance(dist)))
    }
    
    sample_df <- cbind(df, preds)
    
    if (first_location){
      full_df <- sample_df
      first_location <- FALSE
    }
    else {
      full_df <- rbind(full_df, sample_df)
    }
    #print(paste("Forecasted values:", paste(df[, "sample_0", drop=TRUE], collapse = ", ")))
  }
  write.csv(full_df, predictions_fn, row.names = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 4) {
  model_fn <- args[1]
  historic_data_fn <- args[2]
  future_climatedata_fn <- args[3]
  predictions_fn <- args[4]
  
  predict_chap(model_fn, historic_data_fn, future_climatedata_fn, predictions_fn)
}

#testing:
# 
# predict_chap("output/model.bin", "input/trainData.csv", "input/futureClimateData.csv", "output/predictions.csv")
# 
# future_per_location <- get_df_per_location("input/futureClimateData.csv")
# historic_per_location <- get_df_per_location("input/trainData.csv")
# models <- readRDS("output/model.bin")
# location <- names(future_per_location)[1]
# 
# df <- future_per_location[[location]]
# historic_df <- historic_per_location[[location]]
# model <- models[[location]]
# 
# df <- mutate(df, date = yearmonth(date)) #so tsibble understands it is monthly data, fails with exact date
# df <- create_lagged_feature(df, "rain_mm", 3, include_all = FALSE)$df
# df <- create_lagged_feature(df, "temp_c", 3, include_all = FALSE)$df
# df <- fill_top_rows_from_historic_last_rows("rain_mm", 3, df, historic_df)
# df <- fill_top_rows_from_historic_last_rows("temp_c", 3, df, historic_df)
# 
# df_tsibble_new <- as_tsibble(df, index = date)
# 
# predicted_dists <- forecast(model, new_data = df_tsibble_new)
# 
# preds <- data.frame(matrix(ncol = 100, nrow = nrow(df_tsibble_new)))  # 5 rows as an example
# 
# # Assign column names
# colnames(preds) <- paste("sample", 1:100, sep = "_")
# 
# for(i in 1:nrow(df_tsibble_new)){
#   dist <- predicted_dists[i, "n_palu"]$n_palu
#    preds[i,] <- rnorm(100, mean = mean(dist), sd = sqrt(variance(dist)))
# }
# 
# sample_df <- cbind(df, preds)
# 

