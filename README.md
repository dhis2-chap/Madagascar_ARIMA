# Madagascar_ARIMA
This is an implementation of the old ARIMA model for malaria from the Madagascar group. 
The original code is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/scripts/csb-cases/03_forecast-models/arima-model-old.qmd?ref_type=heads
and the data is from https://gitlab.com/pivot-dev/PRIDE-C/pridec-model/-/blob/main/data/for-model/csb-cases/malaria-u5-modelData.csv.
The goal is to integrate the ARIMA model they have made, so it can function through CHAP. 
I decided not to use any of the plotting as CHAP does this on its own (I think).

## Data
The dataset already has $43$ features. However, we only use three of them to fit the model, as well as indexes for location and time. 
In data_preperation.R I remove the unneccessary features, some might be used in other models. Then i split the data into training data and test data, where we later predict the values for the test data. These are saved as csv files called trainData and futureClimateData respectively. Reducing the dataset is not neccessary, but makes it a lot easier to work with.


## Training
We source some useful helper functions from utils.R, similar functions exist in other R packages. Not necessary to use utils.R. We then make the train_chap funcion which calls train_single_region for each seperate location. 
```
train_single_region <- function(df, location){
  df <- mutate(df, date = yearmonth(date)) #so tsibble understands it is monthly data
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
We then get the mean and variation of the normal distribuition and make 100, should maybe be 1000, samples for each prediction. We then columnbind all the predictions for the different locations and write the final dataframe to a csv file.



## Changes
* The function integral failed for me, but lubridate::interval worked.
* 
