# Predicting Food-Borne Illness Outbreak in US States (2016-2026)
### _predicting incriminating foods and toxins for risk mitigation_

[![Web App](https://img.shields.io/badge/Web%20App-View%20Dashboard-blue)](https://ye2qsj-temi.shinyapps.io/my_food_app/)

This project predicts food-related illness risks from 2016-2026 using historical data from 1998-2015. The dashboard provides interactive visualizations to help identify high-risk foods, toxins, and geographic areas for food safety planning and prevention.

## Features

- **State-specific predictions** for all US states
- **Three prediction categories**: Food items, Toxins, and Total Illnesses
- **Dual modeling approach**: ARIMA and ETS forecasting methods
- **Interactive filtering** by year, month, and state
- **Risk rankings** to identify highest-risk items
- **Time series visualization** showing trends from 2016-2026

## Key Components

### 1. Food Risk Analysis
Predicts which food items are most likely to cause foodborne illnesses, ranked by risk level with state-specific insights.

### 2. Toxin Risk Analysis  
Identifies toxins most commonly associated with food outbreaks, helping prioritize food safety testing.

### 3. Illness Predictions
Forecasts total number of food-related illnesses expected, enabling resource planning for healthcare systems.

## Methodology

The project uses time series forecasting models trained on historical food outbreak data from 1998-2015 to predict risks for 2016-2026. Both ARIMA (AutoRegressive Integrated Moving Average) and ETS (Error, Trend, Seasonal) models are employed to provide robust predictions.

## Project Structure

- **[app.R](app.R)** - Main Shiny dashboard application
- **[deploy_app.R](https://github.com/temidataspot/food-illness-prediction/blob/main/deploy_app.R)** - Web deployment to Shiny
- **[CSVs](https://github.com/temidataspot/food-illness-prediction/tree/main/CSVs-Predictions_Performance)** - Folder containing all prediction data files
- **[scripts](https://github.com/temidataspot/food-illness-prediction/tree/main/R-Scripts)** - Analysis scripts for model building
- **[RDS]([docs/](https://github.com/temidataspot/food-illness-prediction/tree/main/RDS-Files))** - RDS Files
  
## Usage

1. Visit the [Web Application](https://ye2qsj-temi.shinyapps.io/my_food_app/)
2. Select your preferred model (ARIMA or ETS)
3. Filter by state, year range, and specific months
4. Explore the three main views: Foods, Toxins, and Illnesses
5. Identify high-risk items 

## Data Sources

- Kaggle

## Dashboard Views

### Combined Risk Overview
Central dashboard showing top high-risk foods and toxins alongside illness predictions.

### Food Risk Predictions
Bar chart ranking food items by predicted risk level.

### Toxin Risk Predictions
Bar chart ranking toxins by predicted risk level.

### Illness Trend Analysis
Time series showing predicted illness counts over the forecast period.

## Technical Details

- Built with R and Shiny framework
- Uses `forecast` package for ARIMA and ETS modeling
- Interactive visualisations with `plotly`
- State-specific analysis capabilities
- Responsive design for all device sizes

## Modelling Scripts

- **[food_analysis.R](https://github.com/temidataspot/food-illness-prediction/blob/main/R-Scripts/Food_model.R)** - Food prediction modeling script
- **[toxin_analysis.R](https://github.com/temidataspot/food-illness-prediction/blob/main/R-Scripts/Toxin_model.R)** - Toxin prediction modeling script
- **[illness_analysis.R](https://github.com/temidataspot/food-illness-prediction/blob/main/R-Scripts/Illness_model.R)** - Illness prediction modeling script

## 🤝 Contributing

This project is maintained for public health and food safety research. Suggestions for improvements or additional features are welcome through GitHub issues.

## 📄 License

This project is available for educational and research purposes. Please cite appropriately when using the data or methods.

---

*Built with ❤️ for food safety and public health*
