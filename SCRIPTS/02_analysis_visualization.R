
# 02_analysis_visualization.R

#install.packages(c("ggplot2", "corrplot", "PerformanceAnalytics", "xts"))

# Load necessary packages for analysis and visualization
library(readr)     # For reading CSV files
library(dplyr)     # For data manipulation
library(ggplot2)   # For powerful data visualization
library(tidyr)     # For tidying data (if needed, already loaded but good practice)
library(lubridate) # For date handling (already loaded but good practice)
library(corrplot)  # For visualizing correlations
library(PerformanceAnalytics) # For financial charting and performance (optional, but good)
library(xts)       # For time series objects (if using PerformanceAnalytics charts)

# --- Step 13: Loading the Integrated Data and Initial Exploration ---

message("\n--- Step 13: Loading the Integrated Data and Initial Exploration ---")

ticker_symbol <- "AAPL" # Ensure this matches your data file

# Load the integrated data
integrated_data_df <- read_csv(file.path("data", paste0(ticker_symbol, "_integrated_financial_data.csv"))) %>%
  mutate(Trading_Date = ymd(Trading_Date)) # Ensure Date is proper date type

message("\nStructure of the integrated data:")
print(str(integrated_data_df))

message("\nSummary statistics of the integrated data:")
print(summary(integrated_data_df))

message("\nFirst few rows of the integrated data:")
print(head(integrated_data_df))

message("\nLast few rows of the integrated data:")
print(tail(integrated_data_df))

# Check for missing values (especially important for merged data)
message("\nMissing values per column:")
print(colSums(is.na(integrated_data_df)))

# --- Step 14: Basic Visualizations ---

message("\n--- Step 14: Basic Visualizations ---")

# 14.1 Plot Adjusted Close Price over Time
plot_price <- ggplot(integrated_data_df, aes(x = Trading_Date, y = Adjusted)) +
  geom_line(color = "steelblue") +
  labs(
    title = paste0(ticker_symbol, " Adjusted Close Price Over Time"),
    x = "Date",
    y = "Adjusted Close Price"
  ) +
  theme_minimal()

print(plot_price)
ggsave(file.path("plots", paste0(ticker_symbol, "_adjusted_close_price.png")), plot = plot_price, width = 10, height = 6)
message(paste0("Adjusted close price plot saved to plots/", ticker_symbol, "_adjusted_close_price.png"))

# 14.2 Plot Daily Log Returns over Time
plot_returns <- ggplot(integrated_data_df, aes(x = Trading_Date, y = Log_Return)) +
  geom_line(color = "darkgreen") +
  labs(
    title = paste0(ticker_symbol, " Daily Log Returns Over Time"),
    x = "Date",
    y = "Log Return"
  ) +
  theme_minimal()

print(plot_returns)
ggsave(file.path("plots", paste0(ticker_symbol, "_daily_log_returns.png")), plot = plot_returns, width = 10, height = 6)
message(paste0("Daily log returns plot saved to plots/", ticker_symbol, "_daily_log_returns.png"))

# 14.3 Plot Daily Average Sentiment over Time (Note: will look very sparse with only 3 articles)
# Filter for days where there was actual news to make the plot meaningful
sentiment_for_plot <- integrated_data_df %>%
  filter(Num_Articles_That_Day > 0) # Only plot days where we have news sentiment

plot_sentiment <- ggplot(sentiment_for_plot, aes(x = Trading_Date, y = Daily_Avg_Sentiment)) +
  geom_line(color = "purple") +
  geom_point(color = "purple") + # Add points for individual news days
  labs(
    title = paste0(ticker_symbol, " Daily Average News Sentiment Over Time"),
    x = "Date",
    y = "Average Sentiment Score"
  ) +
  theme_minimal()

print(plot_sentiment)
ggsave(file.path("plots", paste0(ticker_symbol, "_daily_avg_sentiment.png")), plot = plot_sentiment, width = 10, height = 6)
message(paste0("Daily average sentiment plot saved to plots/", ticker_symbol, "_daily_avg_sentiment.png"))

# --- Step 15: Correlation Analysis (Sentiment vs. Returns) ---

message("\n--- Step 15: Correlation Analysis (Sentiment vs. Returns) ---")

# Select relevant columns for correlation
correlation_data <- integrated_data_df %>%
  dplyr::select(Log_Return, Daily_Avg_Sentiment, Daily_Sum_Sentiment, Num_Articles_That_Day) %>%
  filter(!is.na(Log_Return)) # Ensure no NA returns (already handled, but defensive)

# Calculate correlation matrix
message("\nCorrelation matrix for selected variables:")
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs") # Handles NAs by pairwise deletion
print(correlation_matrix)

# Ensure the 'plots' directory exists
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Save correlation plot
png(file.path("plots", paste0(ticker_symbol, "_correlation_matrix.png")), width = 800, height = 800)
# MODIFIED LINE BELOW: Removed 'order = "hclust"' because it can't handle NAs due to zero std dev
corrplot(correlation_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.8, # Font size of coefficients
         title = paste0("\nCorrelation Matrix for ", ticker_symbol, " Financial Data"))
dev.off() # Close the PNG device
message(paste0("Correlation matrix plot saved to plots/", ticker_symbol, "_correlation_matrix.png"))

message("\n--- Analysis and Visualization Setup COMPLETE! ---")



# 02_analysis_visualization.R

# --- Phase 5: Initial Modeling and Interpretation ---

# --- Step 16: Prepare Data for Modeling ---

message("\n--- Step 16: Prepare Data for Modeling ---")

# Define target variable: Next day's Log_Return
# We need to shift the Log_Return column up by one day.
# A positive sentiment today might affect tomorrow's return.
model_data_df <- integrated_data_df %>%
  arrange(Trading_Date) %>% # Ensure data is sorted by date
  mutate(
    Next_Day_Log_Return = lead(Log_Return, n = 1) # 'lead' shifts values up
  ) %>%
  # Remove rows with NA for the target variable (last row will be NA)
  filter(!is.na(Next_Day_Log_Return))

message("\nFirst few rows of data prepared for modeling (with Next_Day_Log_Return):")
print(head(model_data_df))
message("\nLast few rows of data prepared for modeling (with Next_Day_Log_Return):")
print(tail(model_data_df))
message(paste("Number of rows for modeling:", nrow(model_data_df)))

# --- Step 17: Define and Train a Simple Linear Regression Model ---

message("\n--- Step 17: Define and Train a Simple Linear Regression Model ---")

# For demonstration, we'll use sentiment and a few technical indicators as predictors.
# Due to limited sentiment data, the sentiment coefficients will likely be very noisy or near zero.
# We will use Log_Return (today's return) and some indicators to predict tomorrow's Log_Return.
# Let's add the sentiment variables as well, even if they're mostly zero for now.

# Select relevant columns for the model, ensuring no NAs in predictors
# Fill any remaining NAs in technical indicators with 0 or a mean/median (for simplicity, 0 here)
# SMA_10, SMA_50, RSI_14, MACD, MACD_Signal could have NAs at the start of the series.
model_data_clean <- model_data_df %>%
  dplyr::select(
    Trading_Date,
    Next_Day_Log_Return,
    Log_Return, # Today's return as a predictor
    Daily_Avg_Sentiment,
    Daily_Sum_Sentiment,
    Num_Articles_That_Day,
    SMA_10, SMA_50, RSI_14, MACD, MACD_Signal
  ) %>%
  # For modeling, we need to handle NAs. We already filled sentiment NAs with 0.
  # Let's fill NAs in technical indicators (which appear at the start of the series) with 0 for simplicity.
  mutate(across(c(SMA_10, SMA_50, RSI_14, MACD, MACD_Signal), ~replace_na(., 0))) %>%
  filter(!is.na(Next_Day_Log_Return)) # Re-filter just in case

# Define the formula for the linear model
# Predicting Next_Day_Log_Return based on today's Log_Return, sentiment, and technical indicators
model_formula <- Next_Day_Log_Return ~ Log_Return + Daily_Avg_Sentiment + Daily_Sum_Sentiment +
  Num_Articles_That_Day + SMA_10 + SMA_50 + RSI_14 + MACD + MACD_Signal

message("\nFitting the linear regression model...")
stock_model <- lm(model_formula, data = model_data_clean)

message("\nModel Summary:")
print(summary(stock_model))

# --- Step 18: Evaluate and Interpret Model (Basic) ---

message("\n--- Step 18: Evaluate and Interpret Model (Basic) ---")

# Examine residuals (the differences between actual and predicted values)
# For time series, residuals should ideally be uncorrelated.
message("\nPlotting Residuals vs. Fitted Values:")
plot_residuals <- ggplot(data.frame(Fitted = fitted(stock_model), Residuals = residuals(stock_model)),
                         aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()
print(plot_residuals)
ggsave(file.path("plots", paste0(ticker_symbol, "_model_residuals.png")), plot = plot_residuals, width = 8, height = 6)
message(paste0("Residuals plot saved to plots/", ticker_symbol, "_model_residuals.png"))


message("\n--- Initial Modeling Setup COMPLETE! ---")

# Save the prepared data (optional, but good for reproducibility)
output_file_model_data <- file.path("data", paste0(ticker_symbol, "_model_data.csv"))
write_csv(model_data_clean, output_file_model_data)
message(paste("\nModel-ready data saved to:", output_file_model_data))
