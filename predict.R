source("utils.R")

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
    
    tot_tible <- rbind(historic_df, df) |> #row-bind them together
      mutate(time_period = yearmonth(time_period)) |> #so tsibble understands it is monthly data, fails with exact date
      create_lagged_feature("rainfall", 3, include_all = FALSE) |>
      create_lagged_feature("mean_temperature", 3, include_all = FALSE) |> 
      as_tsibble(index=time_period)
    
    historic_tible = tot_tible[1:nrow(historic_df),]
    future_tible <- tot_tible[(nrow(historic_df) + 1): nrow(tot_tible),]
    
    model = refit(model, historic_tible)
    predicted_dists <- forecast(model, new_data = future_tible)
    
    n_samples <- 100
    preds <- data.frame(matrix(ncol = n_samples, nrow = nrow(future_tible)))
    
    colnames(preds) <- paste("sample", 0:(n_samples-1), sep = "_")
    
    for(i in 1:nrow(future_tible)){
      dist <- predicted_dists[i, "disease_cases"]$disease_cases
      preds[i,] <- rnorm(n_samples, mean = mean(dist), sd = sqrt(variance(dist)))
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
  full_df["time_period"] <- df["time_period"]
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






