
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
  df <- mutate(df, date = yearmonth(date)) |> #so tsibble understands it is monthly data, fails with exact date
    create_lagged_feature("rain_mm", 3, include_all = FALSE) |>
    create_lagged_feature("temp_c", 3, include_all = FALSE) |>
    cut_top_rows(3)
  
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





