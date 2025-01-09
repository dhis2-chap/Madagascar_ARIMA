
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
  df <- mutate(df, date = yearmonth(date)) #so tsibble understands it is monthly data, fails with exact date
  df <- create_lagged_feature(df, "rain_mm", 3, include_all = FALSE)$df
  df <- create_lagged_feature(df, "temp_c", 3, include_all = FALSE)$df
  df <- cut_top_rows(df, 3)
  
  df_tsibble <- as_tsibble(df, index = date)
  model <- df_tsibble |>
    model(
      ARIMA(n_palu ~ rain_mm_3 + temp_c_3 + net_time)
    )  
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

train_chap("input/trainData.csv", "output/model.bin")

# dataframe_list <- get_df_per_location("input/trainData.csv")
# df <- dataframe_list[[3]]
# df <- mutate(df, date = yearmonth(date)) #so tsibble understands it is monthly data, fails with exact date
# df <- create_lagged_feature(df, "rain_mm", 3, include_all = FALSE)$df
# df <- create_lagged_feature(df, "temp_c", 3, include_all = FALSE)$df
# df <- cut_top_rows(df, 3)
# 
# df_tsibble <- as_tsibble(df, index = date)
# model <- df_tsibble |>
#   model(
#     ARIMA(n_palu ~ rain_mm_3 + temp_c_3 + net_time)
#   )  
# report(model)




