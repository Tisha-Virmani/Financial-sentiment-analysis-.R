
# Install and load necessary packages if you haven't already
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("TTR")
# install.packages("corrplot") # For correlation visualization, optional

library(dplyr)
library(ggplot2)
library(lubridate)
library(TTR)
library(corrplot) # Optional, for visual correlation matrix

message("--- Starting Visualization, Correlation, and Modeling in R (with Lexicon Sentiment) ---")

# --- 1. Load the processed data ---
# Make sure this file is in your working directory or provide the full path
merged_data_path <- "AAPL_final_merged_stock_sentiment_data.csv"
if (file.exists(merged_data_path)) {
  aapl_data <- read.csv(merged_data_path)
  aapl_data$Date <- as.Date(aapl_data$Date) # Ensure Date column is in Date format
  message(paste0("Successfully loaded: ", merged_data_path))
} else {
  stop(paste0("Error: ", merged_data_path, " not found. Please run the previous R script first to generate this file with lexicon sentiment."))
}

message("\n--- Correlation Analysis (with Lexicon Sentiment) ---")
# Select numerical columns for correlation analysis, including the new lexicon sentiment columns
numeric_cols <- aapl_data %>%
  select(Close, Log_Return, Simple_Return, SMA_10, SMA_20,
         Daily_Avg_Lexicon_Sentiment, Daily_Sum_Lexicon_Sentiment, Num_Articles_That_Day, Volume) %>%
  drop_na() # Drop NAs to ensure correlation calculation works

correlation_matrix <- cor(numeric_cols)
message("Correlation Matrix:")
print(round(correlation_matrix, 3))

# Optional: Visualize the correlation matrix if corrplot is installed
# png("correlation_matrix_lexicon_sentiment_plot.png", width = 900, height = 900, res = 100)
# corrplot(correlation_matrix, method = "circle", type = "upper",
#          tl.col = "black", tl.srt = 45, addCoef.col = "black",
#          number.cex = 0.7, cl.cex = 0.7, mar = c(0,0,0,0))
# dev.off()
# message("Correlation plot saved as 'correlation_matrix_lexicon_sentiment_plot.png'")


message("\n--- Visualizations (with Lexicon Sentiment) ---")

# 1. Stock Price and Lexicon Sentiment Time Series Plot
p1_lexicon <- ggplot(aapl_data, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Closing Price")) +
  geom_line(aes(y = Daily_Avg_Lexicon_Sentiment * 50, color = "Daily Avg Lexicon Sentiment (Scaled)")) + # Scale sentiment for visibility
  scale_y_continuous(
    name = "Closing Price",
    sec.axis = sec_axis(~./50, name="Daily Average Lexicon Sentiment") # Secondary axis for sentiment
  ) +
  labs(title = "AAPL Closing Price and Daily Average Lexicon Sentiment Over Time",
       x = "Date",
       y = "Closing Price",
       color = "Legend") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("stock_price_lexicon_sentiment_timeseries.png", plot = p1_lexicon, width = 12, height = 6, units = "in")
message("Stock price and lexicon sentiment time series plot saved as 'stock_price_lexicon_sentiment_timeseries.png'")


# 2. Scatter Plot: Daily Average Lexicon Sentiment vs. Log Returns
p2_lexicon <- ggplot(aapl_data, aes(x = Daily_Avg_Lexicon_Sentiment, y = Log_Return)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", col = "red", se = FALSE) + # Add a linear regression line
  labs(title = "Daily Average Lexicon Sentiment vs. Log Returns",
       x = "Daily Average Lexicon Sentiment",
       y = "Log Return") +
  theme_minimal()

ggsave("lexicon_sentiment_vs_return_scatterplot.png", plot = p2_lexicon, width = 8, height = 6, units = "in")
message("Lexicon sentiment vs. log return scatter plot saved as 'lexicon_sentiment_vs_return_scatterplot.png'")


message("\n--- Modeling: Linear Regression Analysis (with Lexicon Sentiment) ---")

# Ensure there are no NAs in the columns used for modeling
model_data_lexicon <- aapl_data %>%
  select(Log_Return, Daily_Avg_Lexicon_Sentiment, SMA_10, SMA_20) %>%
  drop_na()

if (nrow(model_data_lexicon) > 0) {
  # Build a linear regression model using Daily_Avg_Lexicon_Sentiment
  regression_model_lexicon <- lm(Log_Return ~ Daily_Avg_Lexicon_Sentiment + SMA_10 + SMA_20, data = model_data_lexicon)
  
  message("Linear Regression Model Summary (with Lexicon Sentiment):")
  print(summary(regression_model_lexicon))
} else {
  message("Not enough data points to build a regression model with lexicon sentiment after dropping NAs. Check your data.")
}

message("\n--- R Analysis Script with Lexicon Sentiment COMPLETE! Check your console for summaries and working directory for plots. ---")