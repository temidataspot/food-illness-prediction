# Load required libraries (assuming already loaded)
# library(readxl)
# library(dplyr)
# library(forecast)
# library(lubridate)
# library(tidyr)

# Load and clean data (assuming food_outbreaks_clean is already available)
# If not, uncomment the following:
# food_outbreaks <- read_excel("food_outbreaks.xlsx")
# food_outbreaks_clean <- food_outbreaks %>%
#   filter(!grepl("Unknown", Food, ignore.case = TRUE)) %>%
#   filter(!grepl("Unknown", Toxin, ignore.case = TRUE)) %>%
#   mutate(
#     Month_Num = match(Month, month.name),
#     Date = as.Date(paste(Year, Month_Num, "01", sep = "-"))
#   )

# Get list of all unique foods
all_foods <- unique(food_outbreaks_clean$Food)
cat("Total unique foods:", length(all_foods), "\n")

# Function to create food time series by state
create_food_ts_by_state <- function(data) {
  # Create monthly food occurrence data by state
  food_state_monthly <- data %>%
    group_by(State, Food, Year, Month, Date) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(State, Food, Date)
  
  return(food_state_monthly)
}

# Create food occurrence data by state
food_occurrences_by_state <- create_food_ts_by_state(food_outbreaks_clean)

# Function to create time series for a specific state-food combination
create_state_food_ts <- function(state_name, food_name, data) {
  state_food_data <- data %>%
    filter(State == state_name & Food == food_name) %>%
    arrange(Date)
  
  if(nrow(state_food_data) == 0) {
    return(NULL)
  }
  
  # Create complete time series (monthly from min to max date in entire dataset)
  min_date <- min(data$Date)
  max_date <- max(data$Date)
  date_range <- seq.Date(from = min_date, to = max_date, by = "month")
  
  # Join with complete date range
  state_food_complete <- data.frame(Date = date_range) %>%
    left_join(state_food_data %>% select(Date, count), by = "Date") %>%
    mutate(count = ifelse(is.na(count), 0, count))
  
  # Convert to time series
  ts_data <- ts(state_food_complete$count,
                start = c(year(min_date), month(min_date)),
                frequency = 12)
  
  return(list(
    ts = ts_data,
    data = state_food_complete
  ))
}

# Function to build and evaluate models for food time series
build_food_models <- function(ts_data, test_years = 2) {
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

# Focus on the most common foods to make this manageable
# Get top foods by total occurrences
top_foods_by_state <- food_occurrences_by_state %>%
  group_by(Food) %>%
  summarise(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  head(50)  # Top 50 foods to keep it manageable

foods_to_model <- top_foods_by_state$Food
cat("Modeling top", length(foods_to_model), "foods\n")

# Build models for each state-food combination (for top foods)
food_state_models <- list()
processed_combinations <- 0

states_list <- unique(food_outbreaks_clean$State)

for(state in states_list) {
  cat("Processing foods for state:", state, "\n")
  state_food_count <- 0
  
  for(food in foods_to_model) {
    # Create time series for this state-food combination
    ts_result <- create_state_food_ts(state, food, food_occurrences_by_state)
    
    if(!is.null(ts_result)) {
      # Build model
      model_result <- build_food_models(ts_result$ts)
      
      if(!is.null(model_result)) {
        food_state_models[[paste(state, food, sep = "_")]] <- list(
          state = state,
          food = food,
          model = model_result,
          ts_data = ts_result
        )
        state_food_count <- state_food_count + 1
        processed_combinations <- processed_combinations + 1
      }
    }
  }
  
  cat("  Modeled", state_food_count, "food items\n")
}

cat("Total state-food models built:", processed_combinations, "\n")

# Function to make future predictions for food models
# Fixed Function to make future predictions for food models
make_food_predictions <- function(food_state_models, h = 132) {
  predictions <- list()
  
  for(key in names(food_state_models)) {
    model_info <- food_state_models[[key]]
    state <- model_info$state
    food <- model_info$food
    ts_data <- model_info$ts_data$ts
    
    cat("Making predictions for", state, "-", food, "...")
    
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
        food = food,
        arima = arima_pred,
        ets = ets_pred
      )
      
      cat(" Done\n")
    }, silent = FALSE)
  }
  
  return(predictions)
}

# Make future predictions for 2016-2026
food_predictions_2016_2026 <- make_food_predictions(food_state_models, h = 132)

# Create future dates for predictions
future_dates <- seq.Date(from = as.Date("2016-01-01"), 
                         to = as.Date("2026-12-01"), 
                         by = "month")

# Function to create food prediction data frame for Shiny app
# Let's debug and fix the food prediction creation
# First, let's check what's in our food_predictions_2016_2026

cat("Number of food predictions:", length(food_predictions_2016_2026), "\n")

# Let's check the structure of a few predictions
if(length(food_predictions_2016_2026) > 0) {
  first_key <- names(food_predictions_2016_2026)[1]
  cat("First prediction key:", first_key, "\n")
  cat("Structure of first prediction:\n")
  str(food_predictions_2016_2026[[first_key]])
}

# Fixed function to create food prediction data frame
# Fixed function to create food prediction data frame for Shiny app
create_food_prediction_df <- function(food_predictions, future_dates) {
  all_predictions <- data.frame()
  
  cat("Processing", length(food_predictions), "food predictions...\n")
  
  for(key in names(food_predictions)) {
    pred_info <- food_predictions[[key]]
    state <- pred_info$state
    food <- pred_info$food
    
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
      food_df <- data.frame(
        State = rep(state, target_length),
        Food = rep(food, target_length),
        Date = future_dates,
        Year = year(future_dates),
        Month = month(future_dates),
        Month_Name = month.name[month(future_dates)],
        ARIMA_Prediction = pmax(0, arima_mean),  # Ensure non-negative
        ETS_Prediction = pmax(0, ets_mean)       # Ensure non-negative
      )
      
      all_predictions <- rbind(all_predictions, food_df)
      cat("Processed", state, "-", food, "- ARIMA mean:", round(mean(arima_mean), 2), "- ETS mean:", round(mean(ets_mean), 2), "\n")
    }, silent = FALSE)
  }
  
  return(all_predictions)
}
# Try creating the food prediction data frame again
food_predictions_df <- create_food_prediction_df(food_predictions_2016_2026, future_dates)

# Check the result
cat("Food predictions data frame created with", nrow(food_predictions_df), "rows\n")

if(nrow(food_predictions_df) > 0) {
  print("Sample food predictions:")
  print(head(food_predictions_df %>% select(State, Food, Year, Month_Name, ARIMA_Prediction, ETS_Prediction), 10))
  
  # Save to CSV
  write.csv(food_predictions_df, "food_predictions_by_state.csv", row.names = FALSE)
  cat("Food predictions saved to 'food_predictions_by_state.csv'\n")
  
  # Create food risk rankings
  food_risk_rankings <- food_predictions_df %>%
    group_by(State, Food) %>%
    summarise(
      avg_arima_risk = mean(ARIMA_Prediction, na.rm = TRUE),
      avg_ets_risk = mean(ETS_Prediction, na.rm = TRUE),
      max_arima_risk = max(ARIMA_Prediction, na.rm = TRUE),
      max_ets_risk = max(ETS_Prediction, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(State, desc(avg_arima_risk))
  
  # Save risk rankings to CSV
  write.csv(food_risk_rankings, "food_risk_rankings_by_state.csv", row.names = FALSE)
  cat("Food risk rankings saved to 'food_risk_rankings_by_state.csv'\n")
} else {
  cat("Warning: No food predictions were created. Check the modeling process.\n")
}


print("Food predictions created:")
print(paste("States:", length(unique(food_predictions_df$State))))
print(paste("Foods:", length(unique(food_predictions_df$Food))))
print(paste("Total predictions:", nrow(food_predictions_df)))

# Show sample of results
print("Sample food predictions:")
print(head(food_predictions_df %>% select(State, Food, Year, Month_Name, ARIMA_Prediction, ETS_Prediction), 10))

# Save to CSV
write.csv(food_predictions_df, "food_predictions_by_state.csv", row.names = FALSE)

# Create food risk rankings by averaging predictions
food_risk_rankings <- food_predictions_df %>%
  group_by(State, Food) %>%
  summarise(
    avg_arima_risk = mean(ARIMA_Prediction, na.rm = TRUE),
    avg_ets_risk = mean(ETS_Prediction, na.rm = TRUE),
    max_arima_risk = max(ARIMA_Prediction, na.rm = TRUE),
    max_ets_risk = max(ETS_Prediction, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(State, desc(avg_arima_risk))

# Save risk rankings to CSV
write.csv(food_risk_rankings, "food_risk_rankings_by_state.csv", row.names = FALSE)

cat("\nFiles saved:\n")
cat("1. food_predictions_by_state.csv - Monthly food risk predictions\n")
cat("2. food_risk_rankings_by_state.csv - Average risk rankings by state\n")

# Show summary
cat("\n=== FOOD PREDICTIONS SUMMARY ===\n")
cat("States covered:", length(unique(food_predictions_df$State)), "\n")
cat("Foods modeled:", length(unique(food_predictions_df$Food)), "\n")
cat("Prediction period: 2016-2026\n")
cat("Total predictions:", nrow(food_predictions_df), "\n")

# Create and save model performance data for foods
food_model_performance_df <- data.frame()

# Extract performance metrics from food models
for(key in names(food_state_models)) {
  model_info <- food_state_models[[key]]
  state <- model_info$state
  food <- model_info$food
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
          Item = food,
          Model = "ARIMA",
          MAE = arima_mae,
          RMSE = arima_rmse,
          Type = "Food",
          stringsAsFactors = FALSE
        )
        
        ets_perf <- data.frame(
          State = state,
          Item = food,
          Model = "ETS", 
          MAE = ets_mae,
          RMSE = ets_rmse,
          Type = "Food",
          stringsAsFactors = FALSE
        )
        
        food_model_performance_df <- rbind(food_model_performance_df, arima_perf, ets_perf)
      }
    }, silent = TRUE)
  }
}

# Save food model performance to CSV
if(nrow(food_model_performance_df) > 0) {
  write.csv(food_model_performance_df, "food_model_performance.csv", row.names = FALSE)
  cat("Food model performance saved to 'food_model_performance.csv'\n")
  
  # Also create summary of best models
  food_performance_summary <- food_model_performance_df %>%
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
  
  write.csv(food_performance_summary, "food_best_models_by_state.csv", row.names = FALSE)
  cat("Best food models by state saved to 'food_best_models_by_state.csv'\n")
} else {
  cat("Warning: No food model performance data to save\n")
}

cat("\n=== ALL FOOD FILES SAVED ===\n")
cat("1. food_predictions_by_state.csv - Monthly food risk predictions\n")
cat("2. food_risk_rankings_by_state.csv - Average food risk rankings\n")
cat("3. food_model_performance.csv - Food model accuracy metrics\n")
cat("4. food_best_models_by_state.csv - Best performing models by state/food\n")

# Save food predictions in both CSV and RDS formats
if(nrow(food_predictions_df) > 0) {
  # Save as CSV
  write.csv(food_predictions_df, "food_predictions_by_state.csv", row.names = FALSE)
  cat("Food predictions saved to 'food_predictions_by_state.csv'\n")
  
  # Save as RDS
  saveRDS(food_predictions_df, "food_predictions_by_state.rds")
  cat("Food predictions saved to 'food_predictions_by_state.rds'\n")
  
  # Save food risk rankings
  write.csv(food_risk_rankings, "food_risk_rankings_by_state.csv", row.names = FALSE)
  saveRDS(food_risk_rankings, "food_risk_rankings_by_state.rds")
  cat("Food risk rankings saved in CSV and RDS formats\n")
}

# Save model performance in both CSV and RDS formats
if(nrow(food_model_performance_df) > 0) {
  # Save as CSV
  write.csv(food_model_performance_df, "food_model_performance.csv", row.names = FALSE)
  cat("Food model performance saved to 'food_model_performance.csv'\n")
  
  # Save as RDS
  saveRDS(food_model_performance_df, "food_model_performance.rds")
  cat("Food model performance saved to 'food_model_performance.rds'\n")
  
  # Save best models summary
  write.csv(food_performance_summary, "food_best_models_by_state.csv", row.names = FALSE)
  saveRDS(food_performance_summary, "food_best_models_by_state.rds")
  cat("Best food models summary saved in CSV and RDS formats\n")
}

cat("\n=== ALL FOOD FILES SAVED (CSV and RDS) ===\n")
cat("1. food_predictions_by_state.(csv/rds) - Monthly food risk predictions\n")
cat("2. food_risk_rankings_by_state.(csv/rds) - Average food risk rankings\n")
cat("3. food_model_performance.(csv/rds) - Food model accuracy metrics\n")
cat("4. food_best_models_by_state.(csv/rds) - Best performing models by state/food\n")