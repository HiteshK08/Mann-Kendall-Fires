# Trend-Analysis-Fires
This includes Mann Kendall Trend Tests and Linear Regression Analysis of climatic variables.
For Mann Kendall:
install.packages("trend")
install.packages("readr")

library(trend)
library(readr)
data <- read_csv("Correlation1.csv")
# Perform Mann-Kendall test for each column
mk_results <- list()

mk_results$Fire_Occurrences <- MannKendall(data$Fire_Occurrences)
mk_results$Annual_Rainfall <- MannKendall(data$Annual_Rainfall)
mk_results$Wind_Speed <- MannKendall(data$Wind_Speed)
mk_results$Specific_Humidity <- MannKendall(data$Specific_Humidity)
mk_results$Max_Temp <- MannKendall(data$Max_Temp)
mk_results$Min_Temp <- MannKendall(data$Min_Temp)
mk_results$Mean_Diurnal_Range <- MannKendall(data$Mean_Diurnal_Range)
mk_results$Earth_Skin_Temp <- MannKendall(data$Earth_Skin_Temp)

# Print results
mk_results

install.packages("ggplot2")

library(ggplot2)
# Define a function to plot the trend for a given variable
plot_trend <- function(data, variable_name, mk_result) {
  ggplot(data, aes_string(x = "Year", y = variable_name)) +
    geom_line() +
    geom_point() +
    ggtitle(paste("Trend of", variable_name)) +
    theme_minimal() +
    annotate("text", x = min(data$Year), y = max(data[[variable_name]], na.rm = TRUE),
             label = paste("Tau:", round(mk_result$tau, 3), 
                           "\nP-value:", round(mk_result$sl, 3)),
             hjust = 0, vjust = 1, size = 3)
}

# Plot each variable
plot_trend(data, "Fire_Occurrences", mk_results$Fire_Occurrences)
plot_trend(data, "Annual_Rainfall", mk_results$Annual_Rainfall)
plot_trend(data, "Wind_Speed", mk_results$Wind_Speed)
plot_trend(data, "Specific_Humidity", mk_results$Specific_Humidity)
plot_trend(data, "Max_Temp", mk_results$Max_Temp)
plot_trend(data, "Min_Temp", mk_results$Min_Temp)
plot_trend(data, "Mean_Diurnal_Range", mk_results$Mean_Diurnal_Range)
plot_trend(data, "Earth_Skin_Temp", mk_results$Earth_Skin_Temp)
