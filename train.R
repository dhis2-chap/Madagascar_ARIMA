
library(tidymodels)

#time series
library(modeltime)
library(multilevelmod)
library(modeltime)
library(timetk)

packageVersion("rlang")

#For explaining ML models
library(vip)
library(iml)

#data wrangling
library(tidyr)
library(lubridate)
library(doParallel)

library(dplyr) #mutate, across, maybe more


#This is for evaluating model fit I believe, uses cross validation
fit_folds <- function(mod_wf, train_to_fit, 
                      cvfolds_to_fit, pred_var_name, 
                      plot = TRUE, return_preds = FALSE){
  
  
  mod_fit <- mod_wf |> 
    fit_resamples(resamples = cvfolds_to_fit,
                  control = control_resamples(save_pred = TRUE),
                  metrics = forecast_metrics)
  
  preds <- train_to_fit |>
    mutate(row_ind = row_number()) |> 
    left_join(mod_fit |>
                unnest(.predictions) |>
                select(fold = id, pred = .pred, row_ind = .row), 
              by = "row_ind") |>
    rename(obs = {{pred_var_name}})
  
  #mean of performance metrics
  perf_mean  <- collect_metrics(mod_fit)
  
  if(return_preds){
    return(list("preds" = preds, "perf" = perf_mean))
  } else {
    return(perf_mean)
  }
}

# |> passes the object as the first argument to the following function
csb_mal <- read.csv("input/malaria-u5-modelData.csv") |> 
  mutate(date = as.Date(date))

# Create Features

#The data is combined but we want to add lags for the dynamic variables and also create some features.
#below we select 24 variables, not sure if we use the rest yet
  
dyn_vars <- c("pev", "runoff", "humidity", "windspeed", "aod", "fireProp", "rain_mm",
              "temp_c", "vill_evi", "vill_mndwi", "vill_ndwigao")
static_vars <- c("prop_res", "prop_rice","wealth_index", "csb_dist", "med_build_100m",
                 "mean_twi", "prop_sink", "elevation", "streamDist", "edu_prim",
                 "imp_latrine", "net_p2")
#ones that are not static but also that we don't need to lag
other_vars <- c("pop")

mal_prep <- csb_mal |>
  #create lags, only lag 3, not 1 and 2
  mutate(across(all_of(dyn_vars), ~ lag(.x, 3), .names = "{.col}_lag3")) |>
  #drop pre july 2016
  filter(date>as.Date("2016-06-01")) |>
  #create month of year(month_season) and overall month divided by 12, so 18-th month -> 1.5
  mutate(month_season = as.factor(lubridate::month(date)),
         month_num  = (lubridate::interval(min(csb_mal$date), date) %/% months(1))/12) |>
  #csb as factor for random effects, csb is the locations(municipals or communes)
  mutate(csb = as.factor(csb)) |>
  #add time since mosquito net distribution, every three years?
  mutate(net_time = case_when(
    date<as.Date("2018-08-01") ~ as.numeric(date - as.Date("2015-08-01")+1)/365,
    date<as.Date("2021-08-01") ~ as.numeric(date - as.Date("2018-08-01")+1)/365,
    #to update when nets distribution happens
    date<as.Date("2025-08-01") ~ as.numeric(date - as.Date("2021-08-01")+1)/365
  )) |>
  #rescale variables, makes new columns for the scaled variables
  mutate(across((all_of(c(paste(dyn_vars, "lag3", sep = "_"), static_vars))), ~ as.numeric(scale(.x, center = TRUE, scale = TRUE)), .names = "{.col}sc"))

#We have now preperad the dataframe with all the desired variables and lag

# Split data, training and test

#Keep last year for out of sample test data

train_data <- mal_prep |>
  filter(date<as.Date("2023-07-01")) |>
  #create CV split identifier (last year of data)
  mutate(year_ct = ceiling(interval("2016-06-01", date)/years(1))) #changed to lubridate::interval from interval

test_data <- mal_prep |>
  filter(date>=as.Date("2023-07-01"))

# CV Splits

#Three years of training and one year of test

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

cv_folds <- create_cv_3yr_split(train_data)







