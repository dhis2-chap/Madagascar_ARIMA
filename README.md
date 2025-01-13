# Madagascar_ARIMA
This is an implementation of the old ARIMA model for malaria from the Madagascar group. 
The original code is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/scripts/csb-cases/03_forecast-models/arima-model-old.qmd?ref_type=heads
and the data is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/data/for-model/csb-cases/malaria-u5-modelData.csv.
The goal is to integrate the ARIMA model they have made, so it can run through CHAP. The framework is a file for training and a seperate file for prediction. They define the functions train_chap and predict_chap and some standard framework underneath for running the model with CHAP. The strictly necessary files are train.R, predict.R, MLproject and requirements.txt. isolated_run.R is only for testing locally. The goal is to have data as the only input and to return samples from the predicted distribuitions for each time point and location. In this implementation I have used $100$ samples for each observation, should be larger in practice.

## Data
The dataset already has 43 features. However, we only use three of them to fit the model, as well as indexes for location and time. 
In data_preperation.R I remove the unneccessary features for this model. Reducing the dataset is not neccessary, but makes it easier to work with. I also rename the features to the naming convention in CHAP, which for instance is "location" instead of "csb"(should be a overview of the naming conventions somewhere). Then I split the data into training data and test data, where we later predict the values for the test data. These are saved as csv files called trainData and futureClimateData respectively.

## Training
We source some useful helper functions from utils.R, similar functions exist in other R packages. Not necessary to use utils.R. We then make the train_chap function which calls train_single_region for each seperate location. We make different models for if net_time is included or not as the tests in CHAP only use climate data, like rainfall and temperature. 
```
train_single_region <- function(df, location){
  df <- mutate(df, time_period = yearmonth(time_period)) |> 
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
```
I change the date to monthly to avoid making a row for every day, this could make the model slightly different as ARIMA now might assumes the months are equispaced, not sure. Then I use helper functions for lag and deleting the top_rows, could use mutate as well. Then fit the ARIMA model through tsibble objects, and returns the fitted model. From train_chap we save all the models in an output folder.

## Predicting
For each location we process the data as below. We add disease_cases to the future dataframe and make the tsibble tot_tible for all historic and future data with lags and the formatting we desire. We then split it back into historic and future tsibbles.
```
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
    
```
The model is then refit with the historic tsibble, which keeps the same coefficients and model, but update the latest time index to the new potentially later date in historic_tible. This is done because historic_tible might contain newer data than the training data used in train.R. We then predict for the new time points. This step will be different for different models, but the goal is to be able to predict for some following months, and also be able have some months with known data between the training data and the data we predict for.
```
model = refit(model, historic_tible)
predicted_dists <- forecast(model, new_data = future_tible)
```
We then get the mean and variation of the normal distribuition and make 100, should maybe be 1000, samples for each prediction. The column-names for the samples need to be "sample_0", "sample_1" and so on. 
```
n_samples <- 100
preds <- data.frame(matrix(ncol = n_samples, nrow = nrow(future_tible)))

colnames(preds) <- paste("sample", 0:(n_samples-1), sep = "_")

for(i in 1:nrow(future_tible)){
  dist <- predicted_dists[i, "disease_cases"]$disease_cases
  preds[i,] <- rnorm(n_samples, mean = mean(dist), sd = sqrt(variance(dist)))
}
```
We then rowbind all the predictions for the different locations and write the final dataframe to a csv file.
## Packages
All packages used in the code must be written in the file requirements.txt, with a package-name at each line. MLproject then creates a docker enviroment with the mentioned packages installed. For testing locally call all the packages from isolated_run.R, or just in train.R and predict.R when coding and testing.

## Transform to log-scale?
I know this is the old ARIMA model, but sampling without log-transforming makes it possible to get negative values. Log-transforming removes this issue.
