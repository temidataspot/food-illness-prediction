# Load required libraries
library(readxl)
library(dplyr)
library(forecast)
library(lubridate)
library(tidyr)

# Load the data
food_outbreaks <- read_excel("food_outbreaks.xlsx")

# Clean the data - remove Unknown values for consistency
food_outbreaks_clean <- food_outbreaks %>%
  filter(!grepl("Unknown", Food, ignore.case = TRUE)) %>%
  filter(!grepl("Unknown", Toxin, ignore.case = TRUE))

# Convert Month to proper format and create Date column
food_outbreaks_clean <- food_outbreaks_clean %>%
  mutate(
    Month_Num = match(Month, month.name),
    Date = as.Date(paste(Year, Month_Num, "01", sep = "-"))
  )

# Create illness data aggregated by state, year, month
illness_by_state_month <- food_outbreaks_clean %>%
  group_by(State, Year, Month, Date) %>%
  summarise(
    total_illnesses = sum(Illnesses, na.rm = TRUE),
    total_outbreaks = n(),
    .groups = 'drop'
  ) %>%
  arrange(State, Date)

# Get list of states
states_list <- unique(food_outbreaks_clean$State)
cat("Number of states:", length(states_list), "\n")

# Function to create time series for a specific state
create_state_ts <- function(state_name, data) {
  state_data <- data %>%
    filter(State == state_name) %>%
    arrange(Date)
  
  if(nrow(state_data) == 0) {
    return(NULL)
  }
  
  # Create complete time series (monthly from min to max date)
  if(nrow(state_data) > 1) {
    date_range <- seq.Date(from = min(state_data$Date), 
                           to = max(state_data$Date), 
                           by = "month")
    
    # Join with complete date range
    state_complete <- data.frame(Date = date_range) %>%
      left_join(state_data %>% select(Date, total_illnesses), by = "Date") %>%
      mutate(total_illnesses = ifelse(is.na(total_illnesses), 0, total_illnesses))
    
    # Convert to time series
    ts_data <- ts(state_complete$total_illnesses,
                  start = c(year(min(state_complete$Date)), month(min(state_complete$Date))),
                  frequency = 12)
    
    return(list(
      ts = ts_data,
      data = state_complete
    ))
  }
  
  return(NULL)
}

# Create time series for all states
state_ts_list <- list()
for(state in states_list) {
  ts_result <- create_state_ts(state, illness_by_state_month)
  if(!is.null(ts_result)) {
    state_ts_list[[state]] <- ts_result
  }
}

cat("Created time series for", length(state_ts_list), "states\n")

# Function to build and evaluate models for a time series
build_evaluate_models <- function(ts_data, test_years = 2) {
  if(length(ts_data) < 24) {  # Need at least 2 years of data
    return(NULL)
  }
  
  # Split into train/test (use last test_years for testing)
  ts_times <- time(ts_data)
  ts_years <- floor(ts_times)
  max_year <- max(ts_years)
  
  # Use last test_years for testing
  test_start_year <- max_year - test_years + 1
  test_start_index <- min(which(ts_years >= test_start_year))
  
  if(test_start_index <= 5) {  # Not enough training data
    return(NULL)
  }
  
  train_data <- window(ts_data, end = ts_times[test_start_index - 1])
  test_data <- window(ts_data, start = ts_times[test_start_index])
  
  if(length(train_data) < 12 || length(test_data) == 0) {
    return(NULL)
  }
  
  # Build models
  arima_model <- try(auto.arima(train_data), silent = TRUE)
  ets_model <- try(ets(train_data), silent = TRUE)
  
  if(inherits(arima_model, "try-error") || inherits(ets_model, "try-error")) {
    return(NULL)
  }
  
  # Make forecasts
  arima_forecast <- forecast(arima_model, h = length(test_data))
  ets_forecast <- forecast(ets_model, h = length(test_data))
  
  # Calculate accuracy
  arima_accuracy <- accuracy(arima_forecast, test_data)
  ets_accuracy <- accuracy(ets_forecast, test_data)
  
  return(list(
    arima_model = arima_model,
    ets_model = ets_model,
    train_data = train_data,
    test_data = test_data,
    arima_forecast = arima_forecast,
    ets_forecast = ets_forecast,
    arima_accuracy = arima_accuracy,
    ets_accuracy = ets_accuracy
  ))
}

# Build models for all states
state_models <- list()
for(state in names(state_ts_list)) {
  cat("Building models for", state, "...")
  model_result <- build_evaluate_models(state_ts_list[[state]]$ts)
  if(!is.null(model_result)) {
    state_models[[state]] <- model_result
    cat(" Done\n")
  } else {
    cat(" Skipped (insufficient data)\n")
  }
}

cat("Built models for", length(state_models), "states\n")

# Function to make future predictions using full dataset
make_state_predictions <- function(state_ts_list, state_models, h = 132) {
  predictions <- list()
  
  for(state in names(state_models)) {
    if(state %in% names(state_ts_list)) {
      cat("Making predictions for", state, "...")
      
      # Use full time series for final prediction
      full_ts <- state_ts_list[[state]]$ts
      
      try({
        # Build final models on full data
        arima_final <- auto.arima(full_ts)
        ets_final <- ets(full_ts)
        
        # Make future predictions
        arima_pred <- forecast(arima_final, h = h)
        ets_pred <- forecast(ets_final, h = h)
        
        predictions[[state]] <- list(
          arima = arima_pred,
          ets = ets_pred
        )
        
        cat(" Done\n")
      }, silent = TRUE)
    }
  }
  
  return(predictions)
}

# Make future predictions for 2016-2026 (132 months)
state_predictions_2016_2026 <- make_state_predictions(state_ts_list, state_models, h = 132)

# Create future dates for predictions
future_dates <- seq.Date(from = as.Date("2016-01-01"), 
                         to = as.Date("2026-12-01"), 
                         by = "month")

# Function to create prediction data frame for Shiny app
create_illness_prediction_df <- function(state_predictions, future_dates) {
  all_predictions <- data.frame()
  
  for(state in names(state_predictions)) {
    arima_mean <- state_predictions[[state]]$arima$mean
    ets_mean <- state_predictions[[state]]$ets$mean
    
    # Ensure we have the right length
    min_length <- min(length(arima_mean), length(ets_mean), length(future_dates))
    
    state_df <- data.frame(
      State = state,
      Date = future_dates[1:min_length],
      Year = year(future_dates[1:min_length]),
      Month = month(future_dates[1:min_length]),
      Month_Name = month.name[month(future_dates[1:min_length])],
      ARIMA_Prediction = arima_mean[1:min_length],
      ETS_Prediction = ets_mean[1:min_length]
    )
    
    all_predictions <- rbind(all_predictions, state_df)
  }
  
  return(all_predictions)
}

# Create final prediction data frame
illness_predictions_df <- create_illness_prediction_df(state_predictions_2016_2026, future_dates)

print("Illness predictions created for states:")
print(unique(illness_predictions_df$State))

# Summary of model performance for states where we have validation
cat("\n=== MODEL PERFORMANCE SUMMARY ===\n")
for(state in names(state_models)) {
  arima_mae <- state_models[[state]]$arima_accuracy[1, "MAE"]
  ets_mae <- state_models[[state]]$ets_accuracy[1, "MAE"]
  
  cat(state, "- ARIMA MAE:", round(arima_mae, 2), "ETS MAE:", round(ets_mae, 2), "\n")
}

# Save the illness predictions for use in Shiny app
saveRDS(illness_predictions_df, "illness_predictions_by_state.rds")
saveRDS(state_models, "illness_state_models.rds")

cat("\nIllness predictions saved to 'illness_predictions_by_state.rds'\n")
cat("Model information saved to 'illness_state_models.rds'\n")

# Save the illness predictions to CSV
write.csv(illness_predictions_df, "illness_predictions_by_state.csv", row.names = FALSE)

# Also save the model performance data for reference
model_performance_df <- data.frame()

for(state in names(state_models)) {
  arima_perf <- state_models[[state]]$arima_accuracy
  ets_perf <- state_models[[state]]$ets_accuracy
  
  state_perf <- data.frame(
    State = state,
    Model = c("ARIMA", "ETS"),
    MAE = c(arima_perf[1, "MAE"], ets_perf[1, "MAE"]),
    RMSE = c(arima_perf[1, "RMSE"], ets_perf[1, "RMSE"]),
    MAPE = c(arima_perf[1, "MAPE"], ets_perf[1, "MAPE"]),
    stringsAsFactors = FALSE
  )
  
  model_performance_df <- rbind(model_performance_df, state_perf)
}

# Save model performance to CSV
write.csv(model_performance_df, "illness_model_performance.csv", row.names = FALSE)

cat("Files saved:\n")
cat("1. illness_predictions_by_state.csv - Main predictions data\n")
cat("2. illness_model_performance.csv - Model accuracy metrics\n")

# Show summary of what was saved
cat("\n=== SAVED DATA SUMMARY ===\n")
cat("Illness Predictions CSV:\n")
cat("- Columns: State, Date, Year, Month, Month_Name, ARIMA_Prediction, ETS_Prediction\n")
cat("- Rows:", nrow(illness_predictions_df), "\n")
cat("- States:", length(unique(illness_predictions_df$State)), "\n")
cat("- Years: 2016-2026\n")

cat("\nModel Performance CSV:\n")
cat("- Columns: State, Model, MAE, RMSE, MAPE\n")
cat("- Rows:", nrow(model_performance_df), "\n")

# Show sample of the saved data
cat("\n=== SAMPLE DATA ===\n")
print("First 5 rows of illness predictions:")
print(head(illness_predictions_df, 5))

print("Model performance summary:")
print(head(model_performance_df, 10))