# Madagascar_ARIMA
This is an implementation of the old ARIMA model for malaria from the Madagascar group. 
The original code is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/scripts/csb-cases/03_forecast-models/arima-model-old.qmd?ref_type=heads
and the data is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/data/for-model/csb-cases/malaria-u5-modelData.csv.
The goal is to integrate the ARIMA model they have made, so it can run through CHAP. The framework is a file for training and a seperate file for prediction. They define the functions train_chap and predict_chap and some standard framework underneath for running the model with CHAP. isolated_run.R is only for testing locally. The goal is to have data as the only input and to return samples from the predicted distribuitions for each time point and location. In this implementation I have used $100$ samples for each observation, should be larger in practice.

## Data
The dataset already has 43 features. However, we only use three of them to fit the model, as well as indexes for location and time. 
In data_preperation.R I remove the unneccessary features for this model. Reducing the dataset is not neccessary, but makes it easier to work with. I also rename the features to the naming convention in CHAP, which for instance is "location" instead of "csb"(should be a overview of the naming conventions somewhere). Then I split the data into training data and test data, where we later predict the values for the test data. These are saved as csv files called trainData and futureClimateData respectively.

## Training
We source some useful helper functions from utils.R, similar functions exist in other R packages. Not necessary to use utils.R. We then make the train_chap function which calls train_single_region for each seperate location. 
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
We similarly preproccess the data for each region and then predict with the forecast function as below.
```
predicted_dists <- forecast(model, new_data = df_tsibble_new)

preds <- data.frame(matrix(ncol = 100, nrow = nrow(df_tsibble_new)))
    
    colnames(preds) <- paste("sample", 1:100, sep = "_")
    
    for(i in 1:nrow(df_tsibble_new)){
      dist <- predicted_dists[i, "n_palu"]$n_palu
      preds[i,] <- rnorm(100, mean = mean(dist), sd = sqrt(variance(dist)))
    }
```
We then get the mean and variation of the normal distribuition and make 100, should maybe be 1000, samples for each prediction. We then cbind all the predictions for the different locations and write the final dataframe to a csv file. Her we rename the columnnames "date" to "time_period" and "csb" to "location". Might also need to reformat the "time_period" column to 2023-01 instead of 2023 Jan, not quite sure.

## Packages and docker images
Currently all the nedded packages are called in the Rscripts. Should ideally be some docker enviroment.
