# Save this as app.R
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load only the prediction data files
illness_predictions <- readRDS("illness_predictions_by_state.rds")
food_predictions <- readRDS("food_predictions_by_state.rds")
toxin_predictions <- readRDS("toxin_predictions_by_state.rds")

# Get unique states for dropdown
all_states <- unique(c(
  unique(illness_predictions$State),
  unique(food_predictions$State),
  unique(toxin_predictions$State)
))

# Simplified UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Food Illness Risk Dashboard"),
  
  dashboardSidebar(
    # Model selection
    selectInput("model_choice", "Model Type:",
                choices = list("ARIMA" = "ARIMA", "ETS" = "ETS"),
                selected = "ARIMA"),
    
    # Filters
    selectInput("selected_state", "Select State:",
                choices = c("All States" = "all", sort(all_states)),
                selected = "all"),
    
    sliderInput("year_range", "Select Year Range:",
                min = 2016, max = 2026, value = c(2016, 2026), sep = ""),
    
    # Month selection
    checkboxGroupInput("selected_months", "Select Months:",
                       choices = month.name,
                       selected = month.name,
                       inline = TRUE)
  ),
  
  dashboardBody(
    fluidRow(
      # Central combined view
      box(title = "Combined Risk Overview", status = "primary", solidHeader = TRUE,
          plotlyOutput("combined_view", height = 500), width = 12)
    ),
    
    fluidRow(
      # Individual views
      box(title = "Food Risk Predictions", status = "warning", solidHeader = TRUE,
          plotlyOutput("food_view", height = 400), width = 4),
      
      box(title = "Toxin Risk Predictions", status = "danger", solidHeader = TRUE,
          plotlyOutput("toxin_view", height = 400), width = 4),
      
      box(title = "Illness Predictions", status = "info", solidHeader = TRUE,
          plotlyOutput("illness_view", height = 400), width = 4)
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    # Filter illness data
    illness_data <- illness_predictions %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      filter(Month_Name %in% input$selected_months)
    
    if(input$selected_state != "all") {
      illness_data <- illness_data %>% filter(State == input$selected_state)
    }
    
    # Filter food data
    food_data <- food_predictions %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      filter(Month_Name %in% input$selected_months)
    
    if(input$selected_state != "all") {
      food_data <- food_data %>% filter(State == input$selected_state)
    }
    
    # Filter toxin data
    toxin_data <- toxin_predictions %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2]) %>%
      filter(Month_Name %in% input$selected_months)
    
    if(input$selected_state != "all") {
      toxin_data <- toxin_data %>% filter(State == input$selected_state)
    }
    
    list(
      illness = illness_data,
      food = food_data,
      toxin = toxin_data
    )
  })
  
  # Combined view
  output$combined_view <- renderPlotly({
    data <- filtered_data()
    
    # Get prediction values based on model choice
    if(input$model_choice == "ARIMA") {
      illness_pred <- sum(data$illness$ARIMA_Prediction, na.rm = TRUE)
      # Get top foods and toxins
      top_foods <- data$food %>%
        group_by(Food) %>%
        summarise(avg_risk = mean(ARIMA_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(10)
      
      top_toxins <- data$toxin %>%
        group_by(Toxin) %>%
        summarise(avg_risk = mean(ARIMA_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(10)
    } else {
      illness_pred <- sum(data$illness$ETS_Prediction, na.rm = TRUE)
      # Get top foods and toxins
      top_foods <- data$food %>%
        group_by(Food) %>%
        summarise(avg_risk = mean(ETS_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(10)
      
      top_toxins <- data$toxin %>%
        group_by(Toxin) %>%
        summarise(avg_risk = mean(ETS_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(10)
    }
    
    # Create combined visualization
    p1 <- plot_ly() %>%
      add_trace(x = ~Food, y = ~avg_risk, data = top_foods,
                type = 'bar', name = 'Top Foods', marker = list(color = 'orange')) %>%
      layout(title = "Top 10 High-Risk Foods",
             xaxis = list(title = "Food Items"),
             yaxis = list(title = "Risk Score"))
    
    p2 <- plot_ly() %>%
      add_trace(x = ~Toxin, y = ~avg_risk, data = top_toxins,
                type = 'bar', name = 'Top Toxins', marker = list(color = 'red')) %>%
      layout(title = "Top 10 High-Risk Toxins",
             xaxis = list(title = "Toxin Types"),
             yaxis = list(title = "Risk Score"))
    
    p3 <- plot_ly(x = c("Total Predicted Illnesses"), 
                  y = c(illness_pred),
                  type = 'bar', 
                  name = 'Illnesses',
                  marker = list(color = 'blue')) %>%
      layout(title = "Total Predicted Illnesses",
             xaxis = list(title = ""),
             yaxis = list(title = "Number of Cases"))
    
    # Combine plots (this is a simple approach - you might want to use subplot)
    subplot(p1, p2, p3, nrows = 1, margin = 0.05)
  })
  
  # Food view
  output$food_view <- renderPlotly({
    data <- filtered_data()$food
    
    if(nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Get top foods by average risk
    if(input$model_choice == "ARIMA") {
      top_foods <- data %>%
        group_by(Food) %>%
        summarise(avg_risk = mean(ARIMA_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(15)
    } else {
      top_foods <- data %>%
        group_by(Food) %>%
        summarise(avg_risk = mean(ETS_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(15)
    }
    
    plot_ly(data = top_foods, x = ~avg_risk, y = ~reorder(Food, avg_risk),
            type = 'bar', orientation = 'h', 
            marker = list(color = 'orange')) %>%
      layout(title = "Top High-Risk Foods",
             xaxis = list(title = "Risk Score"),
             yaxis = list(title = ""))
  })
  
  # Toxin view
  output$toxin_view <- renderPlotly({
    data <- filtered_data()$toxin
    
    if(nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Get top toxins by average risk
    if(input$model_choice == "ARIMA") {
      top_toxins <- data %>%
        group_by(Toxin) %>%
        summarise(avg_risk = mean(ARIMA_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(15)
    } else {
      top_toxins <- data %>%
        group_by(Toxin) %>%
        summarise(avg_risk = mean(ETS_Prediction, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_risk)) %>%
        head(15)
    }
    
    plot_ly(data = top_toxins, x = ~avg_risk, y = ~reorder(Toxin, avg_risk),
            type = 'bar', orientation = 'h',
            marker = list(color = 'red')) %>%
      layout(title = "Top High-Risk Toxins",
             xaxis = list(title = "Risk Score"),
             yaxis = list(title = ""))
  })
  
  # Illness view
  output$illness_view <- renderPlotly({
    data <- filtered_data()$illness
    
    if(nrow(data) == 0) {
      return(plot_ly() %>% add_annotations(text = "No data", x = 0.5, y = 0.5, showarrow = FALSE))
    }
    
    # Aggregate by year for trend
    if(input$model_choice == "ARIMA") {
      trend_data <- data %>%
        group_by(Year) %>%
        summarise(total = sum(ARIMA_Prediction, na.rm = TRUE), .groups = 'drop')
    } else {
      trend_data <- data %>%
        group_by(Year) %>%
        summarise(total = sum(ETS_Prediction, na.rm = TRUE), .groups = 'drop')
    }
    
    plot_ly(data = trend_data, x = ~Year, y = ~total,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue'),
            marker = list(color = 'blue')) %>%
      layout(title = "Illness Trend Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Cases"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)