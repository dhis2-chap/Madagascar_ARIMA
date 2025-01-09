# Madagascar_ARIMA
This is an implementation of the old ARIMA model for malaria from the Madagascar group. 
The original code is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/scripts/csb-cases/03_forecast-models/arima-model-old.qmd?ref_type=heads
and the data is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/data/for-model/csb-cases/malaria-u5-modelData.csv.
The goal is to integrate the ARIMA model they have made, so it can function through CHAP. 
I decided not to use any of the plotting as CHAP does this on its own (I think).

## Creating and scaling features
The dataset already has $43$ features. And we will make some new ones with 
the function mutate from the dplyr library. Below csb_mal is passed as the first argument to mutate, 
this is what the operator "|>" does. For a list dyn_vars of column names we create lagged columns 
with a lag of three, and the new columns become rainfall_lag3, if it was originaly named rainfall.
```
mal_prep <- csb_mal |>
  mutate(across(all_of(dyn_vars), ~ lag(.x, 3), .names = "{.col}_lag3"))
```
We also make some new columns, like month_season and year_ct to easily access the desired data. 
In total, we end up with $81$ features in the train_data.

We split the training into four different folds where each fold has three years to train on 
and one year to test on. (this could maybe be more general as it will not use data for 
the coming year even if supplied, ie. for year 8)
```
create_cv_3yr_split <- function(data_to_split){
  #create splits using rsamples [3 years to train and 1 to test]
  indices <- list(
    list(analysis = which(data_to_split$year_ct %in% c(1,2,3)),
         assessment = which(data_to_split$year_ct == 4)),
    list(analysis = which(data_to_split$year_ct %in% 2:4),
         assessment = which(data_to_split$year_ct == 5)),
    list(analysis = which(data_to_split$year_ct %in% 3:5),
         assessment = which(data_to_split$year_ct == 6)),
    list(analysis = which(data_to_split$year_ct %in% 4:6),
         assessment = which(data_to_split$year_ct == 7))
  )
  splint_inds <- lapply(indices, make_splits, data = data_to_split)
  cv_folds <- manual_rset(splint_inds, c("Fold2020", "Fold2021", "Fold2022", "Fold2023"))
  
  return(cv_folds)
}

From the object cv_folds we can access the data as training(cv_folds$splits[[1]]) and testing(cv_folds$splits[[2]]) to 
get the training data for the first fold or the test data for the second fold. (This is maybe only used for model evaluation?)

## The model
We use the function ARIMA() from the fable package which automatically fits an ARIMA model to the supplied training data. Below is how 
it was done in the script, which only trains on years 3, 4, 5 and for three random municipalities(csb). We then fit one baseline model 
and one with lagged covariates for rain and temperature and the time since last mosquito net distribuition, which is every three years in 
Madagascar. (seems like a lot of the data processing, with new and lagged columns, was unneccessary for the modelling in this case, 
maybe useful more generaly)
```
fit <- train_ts |>
  filter(year_ct %in% c(3,4,5)) |>
  filter(csb %in% sample(csb,3)) |>
  model(
    auto = ARIMA(n_palu),
    auto_ex = ARIMA(n_palu~rain_mm_lag3 + temp_c_lag3 + net_time)
  )
```
To fit for all years and all municipalities we simply skip the filter lines:
```
fit <- train_ts |>
  model(
    auto = ARIMA(n_palu),
    auto_ex = ARIMA(n_palu~rain_mm_lag3 + temp_c_lag3 + net_time)
  )
```


## Changes
* The function integral failed for me, but lubridate::interval worked.
* 
