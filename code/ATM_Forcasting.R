# Problem 2: ATM Withdrawal Forecasting
# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Load the data
data_path <- "C:\\Users\\RBGH GIS Officer\\OneDrive\\Desktop\\Document\\data_science\\ATM10_data.rds"
atm_data <- readRDS(data_path)

# Convert ymd to Date format
atm_data$ymd <- as.Date(atm_data$ymd)

# ============================================================================
# Create Forecasting Function
# ============================================================================

forecast_atm <- function(data, unit_id, forecast_start = "2009-02-02", 
                         forecast_end = "2009-03-01") {
  
  # Filter data for specific ATM
  unit_data <- data %>% filter(unit == unit_id)
  
  # Add time features
  unit_data <- unit_data %>%
    mutate(
      weekday = wday(ymd),
      day = day(ymd),
      month = month(ymd),
      year_day = yday(ymd)
    )
  
  # Calculate patterns
  # 1. Day of week effect
  weekday_avg <- unit_data %>%
    filter(holiday == 0) %>%
    group_by(weekday) %>%
    summarise(avg_withdrawn = mean(withdrawn, na.rm = TRUE))
  
  # 2. Overall average for holidays
  holiday_avg <- mean(unit_data$withdrawn[unit_data$holiday == 1], na.rm = TRUE)
  
  # 3. Monthly trend
  monthly_avg <- unit_data %>%
    group_by(year, month) %>%
    summarise(avg_withdrawn = mean(withdrawn, na.rm = TRUE), .groups = 'drop')
  
  # 4. Calculate yearly seasonality (for same period last year)
  unit_data <- unit_data %>%
    mutate(month_day = format(ymd, "%m-%d"))
  
  # Create forecast dates
  forecast_dates <- seq(as.Date(forecast_start), as.Date(forecast_end), by = "day")
  
  # Generate forecasts
  forecasts <- data.frame(ymd = forecast_dates)
  forecasts <- forecasts %>%
    mutate(
      weekday = wday(ymd),
      day = day(ymd),
      month = month(ymd),
      month_day = format(ymd, "%m-%d"),
      year = year(ymd)
    )
  
  # Determine if forecast dates are holidays (simplified: weekends as proxy)
  # In reality, you'd need actual holiday calendar
  forecasts$holiday <- ifelse(forecasts$weekday %in% c(1, 7), 1, 0)
  
  # Generate predictions based on patterns
  forecasts$predicted <- NA
  
  for (i in 1:nrow(forecasts)) {
    if (forecasts$holiday[i] == 1) {
      # Use holiday average
      forecasts$predicted[i] <- holiday_avg
    } else {
      # Use weekday average
      wd <- forecasts$weekday[i]
      base_pred <- weekday_avg$avg_withdrawn[weekday_avg$weekday == wd]
      
      # Adjust for same period last year
      same_period_last_year <- unit_data %>%
        filter(month_day == forecasts$month_day[i], year == 2008)
      
      if (nrow(same_period_last_year) > 0) {
        yearly_factor <- same_period_last_year$withdrawn / mean(unit_data$withdrawn[unit_data$year == 2008])
        forecasts$predicted[i] <- base_pred * yearly_factor
      } else {
        forecasts$predicted[i] <- base_pred
      }
      
      # Apply monthly trend adjustment
      recent_month_avg <- mean(unit_data$withdrawn[
        unit_data$month == forecasts$month[i] & 
          unit_data$year == max(unit_data$year)], na.rm = TRUE)
      
      overall_avg <- mean(unit_data$withdrawn, na.rm = TRUE)
      monthly_factor <- recent_month_avg / overall_avg
      
      forecasts$predicted[i] <- forecasts$predicted[i] * monthly_factor
    }
  }
  
  # Ensure predictions are positive
  forecasts$predicted <- pmax(forecasts$predicted, 0)
  
  return(list(historical = unit_data, forecast = forecasts))
}

# ============================================================================
# Generate Forecasts for All ATMs and Create PDF
# ============================================================================

pdf("ATM_Forecasts.pdf", width = 11, height = 8.5)

# Get unique units
units <- sort(unique(atm_data$unit))

# Store all forecasts
all_forecasts <- list()

for (unit_id in units) {
  cat("Processing ATM unit:", unit_id, "\n")
  
  # Generate forecast
  result <- forecast_atm(atm_data, unit_id)
  
  # Store forecast
  all_forecasts[[as.character(unit_id)]] <- result$forecast
  
  # Create plot
  p <- ggplot() +
    # Historical data
    geom_line(data = result$historical, 
              aes(x = ymd, y = withdrawn), 
              color = "steelblue", size = 0.5) +
    geom_point(data = result$historical %>% filter(holiday == 1), 
               aes(x = ymd, y = withdrawn), 
               color = "red", size = 1.5, alpha = 0.5) +
    # Forecast
    geom_line(data = result$forecast, 
              aes(x = ymd, y = predicted), 
              color = "darkgreen", size = 1, linetype = "solid") +
    geom_point(data = result$forecast, 
               aes(x = ymd, y = predicted), 
               color = "darkgreen", size = 2) +
    # Vertical line separating historical and forecast
    geom_vline(xintercept = as.numeric(as.Date("2009-02-01")), 
               linetype = "dashed", color = "black", size = 0.5) +
    # Labels
    labs(title = paste("ATM Unit", unit_id, "- Daily Withdrawals and Forecast"),
         subtitle = "Historical data (blue) | Holidays (red dots) | Forecast (green)",
         x = "Date",
         y = "Withdrawn Amount") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y")
  
  # Add statistics box
  recent_avg <- mean(result$historical$withdrawn[
    result$historical$ymd >= as.Date("2008-12-01")], na.rm = TRUE)
  forecast_avg <- mean(result$forecast$predicted)
  
  stats_text <- paste0(
    "Recent Avg (Dec 2008-Jan 2009): ", round(recent_avg, 0), "\n",
    "Forecast Avg (Feb 2009): ", round(forecast_avg, 0)
  )
  
  p <- p + annotate("text", x = as.Date("2007-06-01"), 
                    y = max(result$historical$withdrawn) * 0.95, 
                    label = stats_text, hjust = 0, size = 3, color = "gray30")
  
  print(p)
}

dev.off()

cat("\nForecasts saved to 'ATM_Forecasts.pdf'\n")

# ============================================================================
# Export Forecast Data
# ============================================================================

# Combine all forecasts into one dataframe
forecast_summary <- bind_rows(lapply(names(all_forecasts), function(unit_id) {
  df <- all_forecasts[[unit_id]]
  df$unit <- as.numeric(unit_id)
  return(df %>% select(unit, ymd, predicted))
}))

# Reshape for easier viewing
forecast_wide <- forecast_summary %>%
  pivot_wider(names_from = unit, 
              values_from = predicted,
              names_prefix = "ATM_")

write.csv(forecast_wide, "ATM_Forecasts_Summary.csv", row.names = FALSE)

cat("Forecast summary saved to 'ATM_Forecasts_Summary.csv'\n")

# Display sample of forecasts
cat("\n=== SAMPLE FORECASTS (First 10 days) ===\n")
print(head(forecast_wide, 10))

# Summary statistics
cat("\n=== FORECAST SUMMARY ===\n")
for (unit_id in units) {
  forecast_values <- all_forecasts[[as.character(unit_id)]]$predicted
  cat("ATM", unit_id, "- Mean:", round(mean(forecast_values), 0), 
      "| Min:", round(min(forecast_values), 0),
      "| Max:", round(max(forecast_values), 0), "\n")
}
