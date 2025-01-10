
source("utils.R")

library(dplyr)
library(fable)
library(tsibble)
library(lubridate)

train_chap <- function(csv_fn, model_fn) {
  dataframe_list <- get_df_per_location(csv_fn)
  
  models <- list()
  for (location in names(dataframe_list)){
    model <- train_single_region(dataframe_list[[location]], location)
    models[[location]] <- model
  }
  saveRDS(models, file=model_fn)
}

train_single_region <- function(df, location){
  df <- mutate(df, time_period = yearmonth(time_period)) |> #so tsibble understands it is monthly data, fails with exact date
    create_lagged_feature("rainfall", 3, include_all = FALSE) |>
    create_lagged_feature("mean_temperature", 3, include_all = FALSE) |>
    cut_top_rows(3)
  
  df_tsibble <- as_tsibble(df, index = time_period)
  
  if ("net_time" %in% colnames(df)){
    model <- df_tsibble |>
      model(
        ARIMA(disease_cases ~ rainfall_3 + mean_temperature_3 + net_time)
      )
  } else {
    model <- df_tsibble |>
      model(
        ARIMA(disease_cases ~ rainfall_3 + mean_temperature_3)
      )
  }
  return(model)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  csv_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(csv_fn, model_fn)
}# else {
#  stop("Usage: Rscript train.R <csv_fn> <model_fn>")
#}



#testing
# 
# dataframe_list <- get_df_per_location("input/training_data.csv")
# df <- dataframe_list[[1]]
# 
# #try without switching to monthly
# df <- create_lagged_feature(df, "rainfall", 3, include_all = FALSE) |>
#   create_lagged_feature("mean_temperature", 3, include_all = FALSE) |>
#   cut_top_rows(3)
# 
# df_tsibble <- as_tsibble(df, index = time_period)
# 
# if ("net_time" %in% colnames(df)){
#   model <- df_tsibble |>
#     model(
#       ARIMA(disease_cases ~ rainfall_3 + mean_temperature_3 + net_time)
#     )
# } else {
#   model <- df_tsibble |>
#     model(
#       ARIMA(disease_cases ~ rainfall_3 + mean_temperature_3)
#     )
# }
# 
# 
# 
# ?forecast




