# Install and load necessary packages if you haven't already
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("TTR")
# install.packages("tidyr")

library(dplyr)
library(lubridate)
library(TTR) # For technical indicators like SMA
library(tidyr) # For replace_na

# --- 1. Load Data ---
# Make sure these CSV files are in your working directory or provide full paths
historical_prices_df <- read.csv("AAPL_historical_prices.csv")
daily_sentiment_df <- read.csv("AAPL_daily_sentiment.csv")
news_data_df_original <- read.csv("AAPL_news_data.csv")
# AAPL_article_sentiment.csv and AAPL_processed_news_data.csv are loaded but not actively used
# in this final merging step, as daily_sentiment is pre-aggregated.

message("--- Starting comprehensive data processing in R ---")

# --- 2. Processing Historical Prices Data ---
message("\nProcessing historical prices data...")
historical_prices_df <- historical_prices_df %>%
  # Convert 'Date' column to Date type
  mutate(Date = as.Date(Date, format="%d-%m-%Y")) %>%
  # Sort by Date
  arrange(Date) %>%
  # Calculate daily log returns
  mutate(Log_Return = log(Close / lag(Close))) %>%
  # Calculate simple returns
  mutate(Simple_Return = (Close - lag(Close)) / lag(Close)) %>%
  # Calculate Simple Moving Averages (SMA)
  mutate(SMA_10 = SMA(Close, n = 10)) %>%
  mutate(SMA_20 = SMA(Close, n = 20)) %>%
  # Remove rows with NA values (from initial lag/SMA calculations)
  na.omit()

message("Processed stock data head:")
print(head(historical_prices_df))
message("Processed stock data structure:")
print(str(historical_prices_df))


# --- 3. Processing Daily Sentiment Data ---
message("\nProcessing daily sentiment data...")
daily_sentiment_df <- daily_sentiment_df %>%
  # Convert 'Trading_Date' column to Date type
  mutate(Date = as.Date(Trading_Date)) %>%
  # Rename 'Trading_Date' to 'Date' for merging
  select(-Trading_Date) # Remove the original Trading_Date column as 'Date' is created

message("Processed daily sentiment data head:")
print(head(daily_sentiment_df))
message("Processed daily sentiment data structure:")
print(str(daily_sentiment_df))


# --- 4. Merging Processed Stock Data with Daily Aggregated Sentiment ---
message("\nMerging stock and sentiment data...")
final_merged_df <- historical_prices_df %>%
  left_join(daily_sentiment_df, by = "Date") %>%
  # Fill NA values for sentiment columns introduced by the merge with 0
  # This handles dates where there might be stock data but no corresponding sentiment
  mutate(Daily_Avg_Sentiment = replace_na(Daily_Avg_Sentiment, 0),
         Daily_Sum_Sentiment = replace_na(Daily_Sum_Sentiment, 0),
         Num_Articles_That_Day = replace_na(Num_Articles_That_Day, 0))

message("Final merged data head:")
print(head(final_merged_df))
message("Final merged data structure:")
print(str(final_merged_df))


# --- 5. Saving the Final Merged DataFrame ---
output_merged_file_name <- "AAPL_final_merged_stock_sentiment_data.csv"
write.csv(final_merged_df, file = output_merged_file_name, row.names = FALSE)
message(paste0("\nFinal merged stock and sentiment data saved to: ", output_merged_file_name))


# --- 6. Additionally, cleaning and saving the original AAPL_news_data.csv ---
message("\nCleaning and saving original news data...")
news_data_cleaned_df <- news_data_df_original %>%
  # Drop 'Unnamed: 8' as it's full of NAs and 'date' as 'timestamp' is more detailed
  select(-matches("Unnamed\\.\\d|date")) %>% # Use matches for pattern
  # Convert 'timestamp' to datetime and extract just the date part
  mutate(timestamp = ymd_hms(timestamp)) %>%
  mutate(Date = as.Date(timestamp)) # Keep only date part

output_cleaned_news_file_name <- "AAPL_cleaned_news_data.csv"
write.csv(news_data_cleaned_df, file = output_cleaned_news_file_name, row.names = FALSE)
message(paste0("Cleaned news data saved to: ", output_cleaned_news_file_name))

message("\n--- All requested data processing COMPLETE in R! ---")