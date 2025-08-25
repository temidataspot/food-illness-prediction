# Loading required libraries 
# Get list of all unique toxins
all_toxins <- unique(food_outbreaks_clean$Toxin)
cat("Total unique toxins:", length(all_toxins), "\n")

# Function to create toxin time series by state
create_toxin_ts_by_state <- function(data) {
  # Create monthly toxin occurrence data by state
  toxin_state_monthly <- data %>%
    group_by(State, Toxin, Year, Month, Date) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(State, Toxin, Date)
  
  return(toxin_state_monthly)
}

# Create toxin occurrence data by state
toxin_occurrences_by_state <- create_toxin_ts_by_state(food_outbreaks_clean)

# Focusing on the most common toxins to make this manageable
# Getting top toxins by total occurrences
top_toxins_by_state <- toxin_occurrences_by_state %>%
  group_by(Toxin) %>%
  summarise(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  head(50)  # Top 50 toxins to keep it manageable

toxins_to_model <- top_toxins_by_state$Toxin
cat("Modeling top", length(toxins_to_model), "toxins\n")

# Function to create time series for a specific state-toxin combination
create_state_toxin_ts <- function(state_name, toxin_name, data) {
  state_toxin_data <- data %>%
    filter(State == state_name & Toxin == toxin_name) %>%
    arrange(Date)
  
  if(nrow(state_toxin_data) == 0) {
    return(NULL)
  }
  
  # Create complete time series (monthly from min to max date in entire dataset)
  min_date <- min(data$Date)
  max_date <- max(data$Date)
  date_range <- seq.Date(from = min_date, to = max_date, by = "month")
  
  # Join with complete date range
  state_toxin_complete <- data.frame(Date = date_range) %>%
    left_join(state_toxin_data %>% select(Date, count), by = "Date") %>%
    mutate(count = ifelse(is.na(count), 0, count))
  
  # Convert to time series
  ts_data <- ts(state_toxin_complete$count,
                start = c(year(min_date), month(min_date)),
                frequency = 12)
  
  return(list(
    ts = ts_data,
    data = state_toxin_complete
  ))
}

# Function to build and evaluate models for toxin time series
build_toxin_models <- function(ts_data, test_years = 2) {
  if(length(ts_data) < 24) {  # Need at least 2 years of data
    return(NULL)
  }
  
  # Split into train/test
  ts_times <- time(ts_data)
  ts_years <- floor(ts_times)
  max_year <- max(ts_years, na.rm = TRUE)
  
  if(is.infinite(max_year) || is.na(max_year)) {
    return(NULL)
  }
  
  # Use last test_years for testing
  test_start_year <- max_year - test_years + 1
  test_indices <- which(ts_years >= test_start_year)
  
  if(length(test_indices) == 0 || min(test_indices) <= 5) {
    return(NULL)
  }
  
  test_start_index <- min(test_indices)
  train_data <- window(ts_data, end = ts_times[test_start_index - 1])
  test_data <- window(ts_data, start = ts_times[test_start_index])
  
  if(length(train_data) < 12 || length(test_data) == 0) {
    return(NULL)
  }
  
  # Build models (only if there's variation in the data)
  if(sd(train_data) > 0 && max(train_data) > 0) {
    arima_model <- try(auto.arima(train_data), silent = TRUE)
    ets_model <- try(ets(train_data), silent = TRUE)
    
    if(!inherits(arima_model, "try-error") && !inherits(ets_model, "try-error")) {
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
  } else {
    # If no variation, create simple mean model
    mean_value <- mean(train_data)
    return(list(
      arima_model = NULL,
      ets_model = NULL,
      train_data = train_data,
      test_data = test_data,
      arima_forecast = list(mean = rep(mean_value, length(test_data))),
      ets_forecast = list(mean = rep(mean_value, length(test_data))),
      arima_accuracy = matrix(c(mean(abs(test_data - mean_value)), sqrt(mean((test_data - mean_value)^2)), mean(abs((test_data - mean_value)/test_data)) * 100), nrow = 1),
      ets_accuracy = matrix(c(mean(abs(test_data - mean_value)), sqrt(mean((test_data - mean_value)^2)), mean(abs((test_data - mean_value)/test_data)) * 100), nrow = 1)
    ))
  }
  
  return(NULL)
}

# Build models for each state-toxin combination (for top toxins)
toxin_state_models <- list()
processed_combinations <- 0

states_list <- unique(food_outbreaks_clean$State)

for(state in states_list) {
  cat("Processing toxins for state:", state, "\n")
  state_toxin_count <- 0
  
  for(toxin in toxins_to_model) {
    # Create time series for this state-toxin combination
    ts_result <- create_state_toxin_ts(state, toxin, toxin_occurrences_by_state)
    
    if(!is.null(ts_result)) {
      # Build model
      model_result <- build_toxin_models(ts_result$ts)
      
      if(!is.null(model_result)) {
        toxin_state_models[[paste(state, toxin, sep = "_")]] <- list(
          state = state,
          toxin = toxin,
          model = model_result,
          ts_data = ts_result
        )
        state_toxin_count <- state_toxin_count + 1
        processed_combinations <- processed_combinations + 1
      }
    }
  }
  
  cat("  Modeled", state_toxin_count, "toxin items\n")
}

cat("Total state-toxin models built:", processed_combinations, "\n")

# Function to make future predictions for toxin models
# Fixed Function to make future predictions for toxin models
make_toxin_predictions <- function(toxin_state_models, h = 132) {
  predictions <- list()
  
  for(key in names(toxin_state_models)) {
    model_info <- toxin_state_models[[key]]
    state <- model_info$state
    toxin <- model_info$toxin
    ts_data <- model_info$ts_data$ts
    
    cat("Making predictions for", state, "-", toxin, "...")
    
    try({
      # Build final models on full data
      arima_final <- NULL
      ets_final <- NULL
      
      # Build ARIMA model
      if(sd(ts_data) > 0 && max(ts_data) > 0) {
        arima_final <- try(auto.arima(ts_data), silent = TRUE)
        ets_final <- try(ets(ts_data), silent = TRUE)
      } else {
        # Simple mean if no variation
        mean_val <- mean(ts_data)
        arima_final <- list(mean = rep(mean_val, h))
        class(arima_final) <- "forecast"  # Set class for proper handling
        ets_final <- list(mean = rep(mean_val, h))
        class(ets_final) <- "forecast"
      }
      
      # Initialize predictions
      arima_pred <- list(mean = rep(0, h))
      ets_pred <- list(mean = rep(0, h))
      
      # Make future predictions for ARIMA
      if(!inherits(arima_final, "try-error") && !is.null(arima_final)) {
        if(inherits(arima_final, "forecast")) {
          # Already a forecast object
          arima_pred <- arima_final
        } else {
          # Need to make forecast
          arima_pred <- try(forecast(arima_final, h = h), silent = TRUE)
          if(inherits(arima_pred, "try-error")) {
            # Fallback to mean
            mean_val <- mean(ts_data, na.rm = TRUE)
            arima_pred <- list(mean = rep(mean_val, h))
            class(arima_pred) <- "forecast"
          }
        }
      } else {
        # Fallback to mean
        mean_val <- mean(ts_data, na.rm = TRUE)
        arima_pred <- list(mean = rep(mean_val, h))
        class(arima_pred) <- "forecast"
      }
      
      # Make future predictions for ETS
      if(!inherits(ets_final, "try-error") && !is.null(ets_final)) {
        if(inherits(ets_final, "forecast")) {
          # Already a forecast object
          ets_pred <- ets_final
        } else {
          # Need to make forecast
          ets_pred <- try(forecast(ets_final, h = h), silent = TRUE)
          if(inherits(ets_pred, "try-error")) {
            # Fallback to mean
            mean_val <- mean(ts_data, na.rm = TRUE)
            ets_pred <- list(mean = rep(mean_val, h))
            class(ets_pred) <- "forecast"
          }
        }
      } else {
        # Fallback to mean
        mean_val <- mean(ts_data, na.rm = TRUE)
        ets_pred <- list(mean = rep(mean_val, h))
        class(ets_pred) <- "forecast"
      }
      
      predictions[[key]] <- list(
        state = state,
        toxin = toxin,
        arima = arima_pred,
        ets = ets_pred
      )
      
      cat(" Done\n")
    }, silent = FALSE)
  }
  
  return(predictions)
}

# Make future predictions for 2016-2026
toxin_predictions_2016_2026 <- make_toxin_predictions(toxin_state_models, h = 132)

# Create future dates for predictions
future_dates <- seq.Date(from = as.Date("2016-01-01"), 
                         to = as.Date("2026-12-01"), 
                         by = "month")

# Fixed function to create toxin prediction data frame for Shiny app
# Fixed function to create toxin prediction data frame for Shiny app
create_toxin_prediction_df <- function(toxin_predictions, future_dates) {
  all_predictions <- data.frame()
  
  cat("Processing", length(toxin_predictions), "toxin predictions...\n")
  
  for(key in names(toxin_predictions)) {
    pred_info <- toxin_predictions[[key]]
    state <- pred_info$state
    toxin <- pred_info$toxin
    
    # Extract predictions more carefully
    try({
      # Handle ARIMA predictions
      arima_mean <- rep(0, length(future_dates))
      if(!is.null(pred_info$arima)) {
        if(inherits(pred_info$arima, "forecast") && !is.null(pred_info$arima$mean)) {
          arima_mean <- as.numeric(pred_info$arima$mean)
        } else if(is.list(pred_info$arima) && !is.null(pred_info$arima$mean)) {
          arima_mean <- as.numeric(pred_info$arima$mean)
        } else {
          # Fallback - use mean of training data
          arima_mean <- rep(1, length(future_dates))  # Default to 1 instead of 0
        }
      }
      
      # Handle ETS predictions
      ets_mean <- rep(0, length(future_dates))
      if(!is.null(pred_info$ets)) {
        if(inherits(pred_info$ets, "forecast") && !is.null(pred_info$ets$mean)) {
          ets_mean <- as.numeric(pred_info$ets$mean)
        } else if(is.list(pred_info$ets) && !is.null(pred_info$ets$mean)) {
          ets_mean <- as.numeric(pred_info$ets$mean)
        } else {
          # Fallback - use mean of training data
          ets_mean <- rep(1, length(future_dates))  # Default to 1 instead of 0
        }
      }
      
      # Ensure we have the right length (132 months for 2016-2026)
      target_length <- length(future_dates)
      
      # Extend or truncate to target length
      if(length(arima_mean) < target_length) {
        arima_mean <- c(arima_mean, rep(tail(arima_mean, 1), target_length - length(arima_mean)))
      } else if(length(arima_mean) > target_length) {
        arima_mean <- arima_mean[1:target_length]
      }
      
      if(length(ets_mean) < target_length) {
        ets_mean <- c(ets_mean, rep(tail(ets_mean, 1), target_length - length(ets_mean)))
      } else if(length(ets_mean) > target_length) {
        ets_mean <- ets_mean[1:target_length]
      }
      
      # Create data frame
      toxin_df <- data.frame(
        State = rep(state, target_length),
        Toxin = rep(toxin, target_length),
        Date = future_dates,
        Year = year(future_dates),
        Month = month(future_dates),
        Month_Name = month.name[month(future_dates)],
        ARIMA_Prediction = pmax(0, arima_mean),  # Ensure non-negative
        ETS_Prediction = pmax(0, ets_mean)       # Ensure non-negative
      )
      
      all_predictions <- rbind(all_predictions, toxin_df)
      cat("Processed", state, "-", toxin, "- ARIMA mean:", round(mean(arima_mean), 2), "- ETS mean:", round(mean(ets_mean), 2), "\n")
    }, silent = FALSE)
  }
  
  return(all_predictions)
}

# Create final toxin prediction data frame
toxin_predictions_df <- create_toxin_prediction_df(toxin_predictions_2016_2026, future_dates)

# Check the result
cat("Toxin predictions data frame created with", nrow(toxin_predictions_df), "rows\n")

if(nrow(toxin_predictions_df) > 0) {
  print("Sample toxin predictions:")
  print(head(toxin_predictions_df %>% select(State, Toxin, Year, Month_Name, ARIMA_Prediction, ETS_Prediction), 10))
  
  # Save to CSV and RDS
  write.csv(toxin_predictions_df, "toxin_predictions_by_state.csv", row.names = FALSE)
  saveRDS(toxin_predictions_df, "toxin_predictions_by_state.rds")
  cat("Toxin predictions saved to 'toxin_predictions_by_state.(csv/rds)'\n")
  
  # Create toxin risk rankings
  toxin_risk_rankings <- toxin_predictions_df %>%
    group_by(State, Toxin) %>%
    summarise(
      avg_arima_risk = mean(ARIMA_Prediction, na.rm = TRUE),
      avg_ets_risk = mean(ETS_Prediction, na.rm = TRUE),
      max_arima_risk = max(ARIMA_Prediction, na.rm = TRUE),
      max_ets_risk = max(ETS_Prediction, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(State, desc(avg_arima_risk))
  
  # Save risk rankings to CSV and RDS
  write.csv(toxin_risk_rankings, "toxin_risk_rankings_by_state.csv", row.names = FALSE)
  saveRDS(toxin_risk_rankings, "toxin_risk_rankings_by_state.rds")
  cat("Toxin risk rankings saved to 'toxin_risk_rankings_by_state.(csv/rds)'\n")
} else {
  cat("Warning: No toxin predictions were created. Check the modeling process.\n")
}

# Create and save model performance data for toxins
toxin_model_performance_df <- data.frame()

# Extract performance metrics from toxin models
for(key in names(toxin_state_models)) {
  model_info <- toxin_state_models[[key]]
  state <- model_info$state
  toxin <- model_info$toxin
  model_results <- model_info$model
  
  if(!is.null(model_results) && !is.null(model_results$arima_accuracy) && !is.null(model_results$ets_accuracy)) {
    try({
      # Extract ARIMA performance - handle different structures
      if(is.matrix(model_results$arima_accuracy)) {
        arima_mae <- model_results$arima_accuracy[1, "MAE"]
        arima_rmse <- model_results$arima_accuracy[1, "RMSE"]
      } else if(is.vector(model_results$arima_accuracy)) {
        arima_mae <- model_results$arima_accuracy[1]
        arima_rmse <- model_results$arima_accuracy[2]
      } else {
        arima_mae <- NA
        arima_rmse <- NA
      }
      
      # Extract ETS performance - handle different structures
      if(is.matrix(model_results$ets_accuracy)) {
        ets_mae <- model_results$ets_accuracy[1, "MAE"]
        ets_rmse <- model_results$ets_accuracy[1, "RMSE"]
      } else if(is.vector(model_results$ets_accuracy)) {
        ets_mae <- model_results$ets_accuracy[1]
        ets_rmse <- model_results$ets_accuracy[2]
      } else {
        ets_mae <- NA
        ets_rmse <- NA
      }
      
      # Only add if we have valid metrics
      if(!is.na(arima_mae) && !is.na(ets_mae)) {
        # Create performance rows
        arima_perf <- data.frame(
          State = state,
          Item = toxin,
          Model = "ARIMA",
          MAE = arima_mae,
          RMSE = arima_rmse,
          Type = "Toxin",
          stringsAsFactors = FALSE
        )
        
        ets_perf <- data.frame(
          State = state,
          Item = toxin,
          Model = "ETS", 
          MAE = ets_mae,
          RMSE = ets_rmse,
          Type = "Toxin",
          stringsAsFactors = FALSE
        )
        
        toxin_model_performance_df <- rbind(toxin_model_performance_df, arima_perf, ets_perf)
      }
    }, silent = TRUE)
  }
}

# Save toxin model performance in both CSV and RDS formats
if(nrow(toxin_model_performance_df) > 0) {
  # Save as CSV
  write.csv(toxin_model_performance_df, "toxin_model_performance.csv", row.names = FALSE)
  cat("Toxin model performance saved to 'toxin_model_performance.csv'\n")
  
  # Save as RDS
  saveRDS(toxin_model_performance_df, "toxin_model_performance.rds")
  cat("Toxin model performance saved to 'toxin_model_performance.rds'\n")
  
  # Also create summary of best models
  toxin_performance_summary <- toxin_model_performance_df %>%
    group_by(State, Item, Model) %>%
    summarise(
      avg_mae = mean(MAE, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    group_by(State, Item) %>%
    summarise(
      best_model = Model[which.min(avg_mae)],
      best_mae = min(avg_mae, na.rm = TRUE),
      .groups = 'drop'
    )
  
  write.csv(toxin_performance_summary, "toxin_best_models_by_state.csv", row.names = FALSE)
  saveRDS(toxin_performance_summary, "toxin_best_models_by_state.rds")
  cat("Best toxin models by state saved in CSV and RDS formats\n")
} else {
  cat("Warning: No toxin model performance data to save\n")
}

cat("\n=== ALL TOXIN FILES SAVED (CSV and RDS) ===\n")
cat("1. toxin_predictions_by_state.(csv/rds) - Monthly toxin risk predictions\n")
cat("2. toxin_risk_rankings_by_state.(csv/rds) - Average toxin risk rankings\n")
cat("3. toxin_model_performance.(csv/rds) - Toxin model accuracy metrics\n")
cat("4. toxin_best_models_by_state.(csv/rds) - Best performing models by state/toxin\n")
