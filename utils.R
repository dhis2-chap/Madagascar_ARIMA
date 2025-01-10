
get_df_per_location <- function(csv_fn){
  full_df <- read.csv(csv_fn)
  locations <- split(full_df, full_df$location)
  return(locations)
}

get_lagged_col_name <- function(feature, lag){
  #feature is a string and lag is an int
  return(paste0(feature, "_", lag))
}

shift <- function(v, lag){
  # v is a vector and lag is an int indicating how many places to shift
  stopifnot(length(v) > lag)
  v <- c(rep(NA, lag), v[1:(length(v) - lag)])
  return(v)
}

create_lagged_feature <- function(df, feature, num_lags, include_all = TRUE){
  if (include_all){
    lag_features <- c()
    for(lag in 1:(num_lags)){
      lag_features <- append(lag_features, get_lagged_col_name(feature, lag))
      df[tail(lag_features, 1)] <- shift(df[[feature]], lag) #use [[]] to get a vector for the given column
    }
  } else{
    lag_features <- c(get_lagged_col_name(feature, num_lags))
    df[tail(lag_features, 1)] <- shift(df[[feature]], num_lags)
  }
  return(df)
}

cut_top_rows <- function(df, int_remove_rows){
  stopifnot(nrow(df) > int_remove_rows)
  return(df[(int_remove_rows + 1):nrow(df),])
}

fill_top_rows_from_historic_last_rows <- function(future_df, historic_df, feature, lag){
  #we fill the lagged columns of future_df for a given feature with the newest historic data, often the training data
  future_df[1:lag, get_lagged_col_name(feature, lag)] <- historic_df[(nrow(historic_df)-lag + 1):nrow(historic_df), feature]
  return(future_df)
}