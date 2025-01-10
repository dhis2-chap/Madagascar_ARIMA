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
    
    df$disease_cases <- NA #so the dataframes have the same columns
    
    tot_df <- rbind(historic_df, df) |> #row-bind them together
      mutate(time_period = yearmonth(time_period)) |> #so tsibble understands it is monthly data, fails with exact date
      create_lagged_feature("rainfall", 3, include_all = FALSE) |>
      create_lagged_feature("mean_temperature", 3, include_all = FALSE)
    
    future_df <- tot_df[(nrow(historic_df) + 1): nrow(tot_df),]
    
    df_tsibble_new <- as_tsibble(future_df, index = time_period)
    
    predicted_dists <- forecast(model, new_data = df_tsibble_new)
    
    preds <- data.frame(matrix(ncol = 100, nrow = nrow(df_tsibble_new)))
    
    colnames(preds) <- paste("sample", 1:100, sep = "_")
    
    for(i in 1:nrow(df_tsibble_new)){
      dist <- predicted_dists[i, "disease_cases"]$disease_cases
      preds[i,] <- rnorm(100, mean = mean(dist), sd = sqrt(variance(dist)))
    }
    
    sample_df <- cbind(future_df, preds)
    
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






