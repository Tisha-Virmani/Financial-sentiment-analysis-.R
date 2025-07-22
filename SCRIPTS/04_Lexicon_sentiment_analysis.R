
# Install and load necessary packages if you haven't already
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("TTR")
# install.packages("tidyr")
# install.packages("stringr") # For string manipulation in sentiment analysis

library(dplyr)
library(lubridate)
library(TTR) # For technical indicators like SMA
library(tidyr) # For replace_na
library(stringr) # For word extraction

message("--- Starting comprehensive data processing with Lexicon Sentiment in R ---")

# --- 1. Define a simplified Loughran-McDonald-like lexicon ---
# IMPORTANT: Replace these with the full Loughran-McDonald lexicon word lists
# if you have them. These are illustrative examples.
positive_words_lm <- c(
  "gain", "profit", "growth", "strong", "increase", "success", "positive",
  "opportunity", "advances", "improvement", "upside", "expansion", "robust",
  "achieve", "benefit", "boost", "confident", "durable", "effective",
  "enhance", "excellent", "favorable", "feasible", "flexible", "growing"
)

negative_words_lm <- c(
  "loss", "decline", "risk", "weak", "decrease", "failure", "negative",
  "challenge", "downside", "reduction", "volatile", "crisis", "fall",
  "adverse", "alleged", "burden", "concern", "costly", "damage",
  "default", "deteriorate", "difficult", "distress", "down", "error"
)

# Function to perform lexicon-based sentiment analysis on a single text
get_lexicon_sentiment_r <- function(text, pos_words, neg_words) {
  if (is.null(text) || !is.character(text) || nchar(text) == 0) {
    return(0) # Neutral sentiment for empty or invalid text
  }
  
  # Convert text to lowercase and tokenize
  words <- str_to_lower(text) %>%
    str_extract_all(pattern = "\\b\\w+\\b") %>%
    unlist()
  
  positive_score <- sum(words %in% pos_words)
  negative_score <- sum(words %in% neg_words)
  
  total_relevant_words <- positive_score + negative_score
  if (total_relevant_words == 0) {
    return(0)
  } else {
    # Simple score: (Positives - Negatives) / (Total relevant words)
    return((positive_score - negative_score) / total_relevant_words)
  }
}


# --- 2. Load Data ---
# Make sure these CSV files are in your working directory or provide full paths
historical_prices_df <- read.csv("AAPL_historical_prices.csv")
news_df_original <- read.csv("AAPL_news_data.csv")

message("\nLoading and preprocessing data...")

# --- 3. Preprocessing Historical Prices Data ---
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


# --- 4. Process News Data for Lexicon Sentiment Analysis ---
message("\nProcessing news data for lexicon sentiment...")

# *** DEBUGGING STEP: Check column names of news_df_original ***
message(paste0("Column names in original news data: ", paste(colnames(news_df_original), collapse = ", ")))

# --- Standardize Timestamp Column ---
if (!"timestamp" %in% colnames(news_df_original)) {
  if ("date" %in% colnames(news_df_original) && is.character(news_df_original$date)) {
    news_df_original <- news_df_original %>% rename(timestamp = date)
    message("Renamed 'date' column to 'timestamp'.")
  } else if ("Date" %in% colnames(news_df_original) && is.character(news_df_original$Date)) {
    news_df_original <- news_df_original %>% rename(timestamp = Date)
    message("Renamed 'Date' column to 'timestamp'.")
  } else {
    stop("Error: Neither 'timestamp', 'date', nor 'Date' column (as character) found in news data. Please check your CSV file's column names for the timestamp/date information.")
  }
}

# --- Standardize News Content Column ---
if (!"news_content" %in% colnames(news_df_original)) {
  if ("NewsContent" %in% colnames(news_df_original) && is.character(news_df_original$NewsContent)) {
    news_df_original <- news_df_original %>% rename(news_content = NewsContent)
    message("Renamed 'NewsContent' column to 'news_content'.")
  } else if ("content" %in% colnames(news_df_original) && is.character(news_df_original$content)) {
    news_df_original <- news_df_original %>% rename(news_content = content)
    message("Renamed 'content' column to 'news_content'.")
  } else if ("text" %in% colnames(news_df_original) && is.character(news_df_original$text)) {
    news_df_original <- news_df_original %>% rename(news_content = text)
    message("Renamed 'text' column to 'news_content'.")
  } else {
    stop("Error: 'news_content', 'NewsContent', 'content', or 'text' column not found in news data. Please check your CSV for the main article text column.")
  }
}

# Now, perform the rest of the processing using the standardized column names
news_df_processed <- news_df_original %>%
  # Ensure news_content is character and fill NA with empty string before sentiment analysis
  mutate(news_content = as.character(news_content)) %>%
  mutate(news_content = ifelse(is.na(news_content), "", news_content)) %>%
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC", quiet = TRUE)) %>% # Using lubridate for robustness
  filter(!is.na(timestamp)) %>% # Remove rows with invalid timestamps
  mutate(Date = as.Date(timestamp)) # Extract just the date


# Apply the lexicon sentiment function to each news article
message("Applying lexicon sentiment to individual articles...")
news_df_processed$Lexicon_Sentiment <- sapply(news_df_processed$news_content,
                                              get_lexicon_sentiment_r,
                                              pos_words = positive_words_lm,
                                              neg_words = negative_words_lm)

# Aggregate lexicon sentiment to daily level
message("Aggregating lexicon sentiment to daily level...")
daily_lexicon_sentiment_df <- news_df_processed %>%
  group_by(Date) %>%
  summarise(
    Daily_Avg_Lexicon_Sentiment = mean(Lexicon_Sentiment, na.rm = TRUE),
    Daily_Sum_Lexicon_Sentiment = sum(Lexicon_Sentiment, na.rm = TRUE),
    Num_Articles_That_Day = n()
  ) %>%
  ungroup()

message("Daily aggregated lexicon sentiment head:")
print(head(daily_lexicon_sentiment_df))
message("Daily aggregated lexicon sentiment structure:")
print(str(daily_lexicon_sentiment_df))


# --- 5. Merge Processed Stock Data with Daily Lexicon Sentiment ---
message("\nMerging stock data with daily lexicon sentiment...")
final_merged_df <- historical_prices_df %>%
  left_join(daily_lexicon_sentiment_df, by = "Date") %>%
  # Fill NA values for lexicon sentiment columns introduced by the merge with 0
  mutate(Daily_Avg_Lexicon_Sentiment = replace_na(Daily_Avg_Lexicon_Sentiment, 0),
         Daily_Sum_Lexicon_Sentiment = replace_na(Daily_Sum_Lexicon_Sentiment, 0),
         Num_Articles_That_Day = replace_na(Num_Articles_That_Day, 0))

message("Final merged data (with lexicon sentiment) head:")
print(head(final_merged_df))
message("Final merged data (with lexicon sentiment) structure:")
print(str(final_merged_df))


# --- 6. Saving the Final Merged DataFrame ---
output_merged_file_name <- "AAPL_final_merged_stock_sentiment_data.csv"
write.csv(final_merged_df, file = output_merged_file_name, row.names = FALSE)
message(paste0("\nFinal merged stock and lexicon sentiment data saved to: ", output_merged_file_name))


# --- 7. Also clean and save the original AAPL_news_data.csv (keeping original sentiment from it) ---
message("\nCleaning and saving original news data (as a separate file)...")
# This part ensures the original news data is also processed and saved cleanly,
# independent of the lexicon sentiment calculation.
news_data_cleaned_df <- news_df_original %>%
  # Only select columns that are NOT named 'Unnamed: X' or 'date' (if 'timestamp' is used)
  select(-matches("Unnamed\\.\\d|date$")) %>% # Added $ to ensure exact match for 'date'
  # Make sure timestamp is present after potential renames
  mutate(timestamp = ymd_hms(timestamp, tz = "UTC", quiet = TRUE)) %>%
  filter(!is.na(timestamp)) %>% # Remove rows with invalid timestamps
  mutate(Date = as.Date(timestamp)) # Keep only date part

output_cleaned_news_file_name <- "AAPL_cleaned_news_data.csv"
write.csv(news_data_cleaned_df, file = output_cleaned_news_file_name, row.names = FALSE)
message(paste0("Cleaned news data saved to: ", output_cleaned_news_file_name))

message("\n--- R Process_appl Script with Lexicon Sentiment COMPLETE! ---")
message("Remember to replace the placeholder lexicon words with the full Loughran-McDonald lexicon for best results.")