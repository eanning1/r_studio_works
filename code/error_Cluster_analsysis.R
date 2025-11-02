# Error Clustering Analysis

#install.packages(c("ggplot2", "dplyr", "tidyr", "lubridate", "gridExtra"))

#install.packages("ggplot2")


# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(gridExtra)

# Load the data
data_path <- "D:\\Codes\\R_Codes\\r_studio_works\\raw_data\\Errors_company01.rds"
errors <- readRDS(data_path)

# Convert error.time to POSIXct if not already
errors$error.time <- as.POSIXct(errors$error.time)

# ============================================================================
# TASK (i): Graphical Overview of the Data
# ============================================================================

# 1. Distribution of errors over time
p1 <- ggplot(errors, aes(x = error.time)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Errors Over Time",
       x = "Date", y = "Number of Errors") +
  theme_minimal()

# 2. Errors by service
service_counts <- errors %>%
  group_by(service) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

p2 <- ggplot(service_counts %>% top_n(20, count), 
             aes(x = reorder(service, count), y = count)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(title = "Top 20 Services by Error Count",
       x = "Service", y = "Number of Errors") +
  theme_minimal()

# 3. Errors by laptop
laptop_counts <- errors %>%
  group_by(laptop) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

p3 <- ggplot(laptop_counts, aes(x = count)) +
  geom_histogram(bins = 30, fill = "darkgreen", alpha = 0.7) +
  labs(title = "Distribution of Errors Across Laptops",
       x = "Number of Errors per Laptop", y = "Frequency") +
  theme_minimal()

# 4. Errors on special days vs regular days
special_day_summary <- errors %>%
  group_by(special.day) %>%
  summarise(count = n()) %>%
  mutate(day_type = ifelse(special.day == 1, "Special Day", "Regular Day"))

p4 <- ggplot(special_day_summary, aes(x = day_type, y = count, fill = day_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Regular Day" = "lightblue", "Special Day" = "orange")) +
  labs(title = "Errors on Special Days vs Regular Days",
       x = "", y = "Number of Errors") +
  theme_minimal() +
  theme(legend.position = "none")

# 5. Hourly pattern of errors
errors$hour <- hour(errors$error.time)
hourly_pattern <- errors %>%
  group_by(hour) %>%
  summarise(count = n())

p5 <- ggplot(hourly_pattern, aes(x = hour, y = count)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 2) +
  labs(title = "Hourly Pattern of Errors",
       x = "Hour of Day", y = "Number of Errors") +
  theme_minimal()

# 6. Day of week pattern
errors$weekday <- wday(errors$error.time, label = TRUE)
weekday_pattern <- errors %>%
  group_by(weekday) %>%
  summarise(count = n())

p6 <- ggplot(weekday_pattern, aes(x = weekday, y = count, fill = weekday)) +
  geom_bar(stat = "identity") +
  labs(title = "Errors by Day of Week",
       x = "Day of Week", y = "Number of Errors") +
  theme_minimal() +
  theme(legend.position = "none")

# Save all plots to PDF
pdf("Error_Analysis_Overview.pdf", width = 11, height = 8.5)
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
dev.off()

cat("Graphical overview saved to 'Error_Analysis_Overview.pdf'\n")

# ============================================================================
# TASK (ii): Identify Trigger Services
# ============================================================================

# Function to find triggered errors within 10 seconds
find_triggers <- function(data, time_window = 10) {
  
  # Sort data by laptop and time
  data <- data %>% arrange(laptop, error.time)
  
  # Initialize results
  trigger_results <- list()
  
  # Get unique services
  services <- unique(data$service)
  
  cat("Analyzing", length(services), "services for trigger patterns...\n")
  
  for (trigger_service in services) {
    # Get all errors for this service
    trigger_errors <- data %>% filter(service == trigger_service)
    
    if (nrow(trigger_errors) < 5) next  # Skip services with too few errors
    
    # For each trigger error, find subsequent errors on the same laptop
    triggered_counts <- list()
    
    for (i in 1:nrow(trigger_errors)) {
      laptop_id <- trigger_errors$laptop[i]
      trigger_time <- trigger_errors$error.time[i]
      
      # Find errors on same laptop within 10 seconds after trigger
      subsequent_errors <- data %>%
        filter(laptop == laptop_id,
               service != trigger_service,
               error.time > trigger_time,
               error.time <= trigger_time + time_window)
      
      if (nrow(subsequent_errors) > 0) {
        for (svc in subsequent_errors$service) {
          if (is.null(triggered_counts[[svc]])) {
            triggered_counts[[svc]] <- 0
          }
          triggered_counts[[svc]] <- triggered_counts[[svc]] + 1
        }
      }
    }
    
    # Calculate trigger probabilities
    n_trigger_events <- nrow(trigger_errors)
    
    for (triggered_service in names(triggered_counts)) {
      count <- triggered_counts[[triggered_service]]
      probability <- count / n_trigger_events
      
      # Only consider as trigger if probability > 0.3 (high probability)
      # and occurred at least 10 times
      if (probability > 0.3 && count >= 10) {
        trigger_results[[length(trigger_results) + 1]] <- data.frame(
          trigger_service = trigger_service,
          triggered_service = triggered_service,
          count = count,
          total_trigger_events = n_trigger_events,
          probability = probability
        )
      }
    }
  }
  
  # Combine results
  if (length(trigger_results) > 0) {
    results_df <- bind_rows(trigger_results) %>%
      arrange(desc(probability))
    return(results_df)
  } else {
    return(NULL)
  }
}

# Find triggers
cat("\nSearching for trigger patterns...\n")
triggers <- find_triggers(errors, time_window = 10)

# Display results
if (!is.null(triggers) && nrow(triggers) > 0) {
  cat("\n=== TRIGGER SERVICES FOUND ===\n\n")
  print(triggers, row.names = FALSE)
  
  # Save to CSV
  write.csv(triggers, "Trigger_Services.csv", row.names = FALSE)
  
  # Create visualization
  p_triggers <- ggplot(triggers %>% top_n(20, probability), 
                       aes(x = reorder(paste(trigger_service, "->", triggered_service), 
                                       probability), 
                           y = probability)) +
    geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
    coord_flip() +
    labs(title = "Top Trigger Relationships (Probability > 0.3)",
         x = "Trigger -> Triggered Service",
         y = "Trigger Probability") +
    theme_minimal()
  
  pdf("Trigger_Services_Visualization.pdf", width = 10, height = 8)
  print(p_triggers)
  dev.off()
  
  cat("\nResults saved to 'Trigger_Services.csv' and 'Trigger_Services_Visualization.pdf'\n")
} else {
  cat("\nNo trigger services found with the specified criteria (probability > 0.3, count >= 10)\n")
}

# Summary statistics
cat("\n=== DATA SUMMARY ===\n")
cat("Total number of errors:", nrow(errors), "\n")
cat("Number of unique laptops:", length(unique(errors$laptop)), "\n")
cat("Number of unique services:", length(unique(errors$service)), "\n")
cat("Date range:", as.character(min(errors$error.time)), "to", 
    as.character(max(errors$error.time)), "\n")
cat("Errors on special days:", sum(errors$special.day == 1), "\n")
cat("Errors on regular days:", sum(errors$special.day == 0), "\n")
