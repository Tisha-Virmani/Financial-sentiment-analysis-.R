
# Note: For deep learning (keras), we'll install it later, as it has more dependencies.
# install.packages("keras") # Don't run this yet.

#install.packages("janitor")
# Load necessary packages
library(dplyr)
library(lubridate)   # Make sure lubridate is loaded early
library(httr)
library(jsonlite)
library(readr)
library(TTR)
library(tidytext)
library(textdata)
library(stringr)
library(tidyr)       # Make sure tidyr is loaded
library(janitor)     # For Alpha Vantage (if you've installed it)


# We'll load text-specific and ML packages later when we need them
library(tidytext)
library(textdata)
library(tidyr)
#library(ggplot2)
# library(tidymodels)



# 01_data_acquisition.R

# --- Step 4: Downloading Historical Stock Price Data ---

# Load quantmod (and other necessary data manipulation packages)
library(quantmod)
library(dplyr)
library(lubridate)
library(readr) # For saving data

# Define the stock ticker and date range
ticker_symbol <- "AAPL"
start_date <- as.Date("2020-01-01")
end_date <- Sys.Date() # Today's date

message(paste("Downloading historical data for", ticker_symbol, "from", start_date, "to", end_date))

# Use getSymbols to download data from Yahoo Finance
# auto.assign=FALSE ensures the data is returned as an object, not directly assigned to the global environment
stock_data_xts <- getSymbols(ticker_symbol,
                             src = "yahoo",
                             from = start_date,
                             to = end_date,
                             auto.assign = FALSE)

# getSymbols returns an xts object. We often prefer data frames (tibbles) for analysis with tidyverse.
# Convert xts object to a tibble (a type of data frame)
stock_data_df <- stock_data_xts %>%
  as.data.frame() %>% # Convert to a regular data frame first
  tibble::rownames_to_column("Date") %>% # Convert row names (dates) to a column
  as_tibble() %>% # Convert to a tibble
  mutate(Date = ymd(Date)) # Ensure Date column is in Date format

# Rename columns for clarity (e.g., AAPL.Open to Open)
# We use stringr::str_replace for a clean rename across columns
stock_data_df <- stock_data_df %>%
  rename_with(~str_replace(., paste0(ticker_symbol, "."), "")) %>%
  # Select only the columns we need: Date, Open, High, Low, Close, Volume, Adjusted
  select(Date, Open, High, Low, Close, Volume, Adjusted)

# Display the first few rows of the data
message("\nFirst 6 rows of stock data:")
print(head(stock_data_df))

# Display the structure of the data
message("\nStructure of stock data:")
print(str(stock_data_df))

# Save the raw stock data to a CSV file in your 'data' folder
output_file_stock <- file.path("data", paste0(ticker_symbol, "_historical_prices.csv"))
write_csv(stock_data_df, output_file_stock)
message(paste("\nStock data saved to:", output_file_stock))



# 01_data_acquisition.R

# (Keep all your previous code for Step 4 - Stock Data Acquisition, including initial library calls)

# --- Step 7: Integrating Marketaux API for News Data ---

# --- Configuration for Marketaux API ---
# IMPORTANT: Replace "YOUR_MARKETAUX_API_KEY" with your actual API key
marketaux_api_key <- "YK55fStbSY6ZUfon8GhrlTzWkiX830ldOndw7LV5" # <--- PASTE YOUR API KEY HERE
base_api_url <- "https://api.marketaux.com/v1/news/all"

# 01_data_acquisition.R

# ... (Previous code including ticker_symbol and api_key setup) ...

# Ensure all necessary packages are loaded at the top of your script.
# The order can sometimes matter for avoiding conflicts, so a common good order is:
# library(dplyr)      # Always useful, often first
# library(lubridate)  # Critical for dates, load early
# library(httr)       # For API calls
# library(jsonlite)   # For JSON parsing
# library(readr)      # For read_csv/write_csv, load before tidyr sometimes
# library(TTR)        # For technical indicators
# library(tidytext)   # For text analysis
# library(textdata)   # For sentiment lexicon
# library(stringr)    # For string manipulation
# library(tidyr)      # For unnest_wider, replace_na, etc.
# library(janitor)    # For clean_names (if using Alpha Vantage)


# --- Step 7: Marketaux News API Integration (Optimized - Minimal Messaging for Debugging) ---

cat("\n--- Step 7: Marketaux News API Integration (Optimized - Starting News Fetch) ---\n")

# Define date range for news fetching.
# Marketaux free tier usually limited to recent history (approx. last 14 days).
# Adjust these dates as needed for your specific API access window.
end_date_fetch <- Sys.Date() # Today's date
start_date_fetch <- Sys.Date() - days(13) # Go back 13 days from today, covering 14 days in total

# Create a sequence of dates to iterate over
date_sequence <- seq(from = start_date_fetch, to = end_date_fetch, by = "day")

# Initialize an empty list to store news data from multiple fetches
all_news_data_list <- list()
request_count <- 0 # Track API requests to respect limits

# Simplified message to avoid date formatting issues
cat(paste0("Attempting to fetch news for ", ticker_symbol, " for the last 14 days...\n"))

for (current_date in date_sequence) {
  # Format dates for API query in ISO 8601 format (YYYY-MM-DDTHH:MM:SS)
  date_after_iso <- format(current_date, "%Y-%m-%dT00:00:00")
  date_before_iso <- format(current_date + days(1), "%Y-%m-%dT00:00:00") # Next day at midnight
  
  marketaux_url <- paste0(
    "https://api.marketaux.com/v1/news/all?",
    "symbols=", ticker_symbol,
    "&filter_entities=true",
    "&language=en",
    "&published_after=", date_after_iso,
    "&published_before=", date_before_iso,
    "&limit=3", # Max 3 articles for free tier per request
    "&api_token=", api_key
  )
  
  # Simplified message to avoid specific date formatting issues within the loop
  cat(paste0("  Fetching news for a day in the range...\n"))
  
  response <- GET(marketaux_url)
  news_json <- content(response, "parsed")
  
  request_count <- request_count + 1
  
  if (!is.null(news_json$data) && length(news_json$data) > 0) {
    temp_df <- as_tibble(news_json$data)
    all_news_data_list[[as.character(current_date)]] <- temp_df # Store with date as key
    cat(paste0("  -> Fetched ", nrow(temp_df), " articles for a day.\n"))
  } else if (!is.null(news_json$error)) {
    warning(paste0("Marketaux API Error for a day: ", news_json$error$message))
  } else {
    cat(paste0("  -> No news found for a day.\n"))
  }
  
  # Add a small delay to avoid hitting API rate limits
  Sys.sleep(0.5) # Pause for 0.5 seconds between requests
  
  # Optional: Implement a check for Marketaux free tier request limit (100 requests/day)
  # If you are fetching for many days, you might hit the limit.
  # if (request_count >= 95) { # Stop a bit before 100 to be safe
  #   cat("Approaching Marketaux free tier request limit. Stopping news fetch early.\n")
  #   break
  # }
}

# Process combined news data after the loop
if (length(all_news_data_list) > 0) {
  news_data_combined <- bind_rows(all_news_data_list) %>%
    # Select and rename relevant columns based on the actual API response
    dplyr::select(
      uuid,               # Keep UUID for robust deduplication
      published_at,
      title,
      description,
      url,
      source,
      entities
    ) %>%
    dplyr::rename(
      UUID = uuid,
      Date = published_at,
      Headline = title,
      Snippet = description,
      Link = url,
      Source = source,
      Entities = entities
    ) %>%
    # Convert 'Date' to proper POSIXct object for consistency
    mutate(Date = ymd_hms(Date, tz = "UTC", quiet = TRUE)) %>%
    # Filter out articles with missing or empty headlines/links
    filter(!is.na(Headline), !is.na(Link), Headline != "") %>%
    # Deduplicate based on UUID to ensure only unique articles
    distinct(UUID, .keep_all = TRUE)
  
  cat(paste0("\nSuccessfully fetched and combined ", nrow(news_data_combined), " new unique news articles.\n"))
  cat("First few of the newly fetched articles:\n")
  print(head(news_data_combined)) # Use print for tibble/data.frame output
  
  # --- Handle Accumulation/Appending to CSV ---
  output_file_marketaux_news <- file.path("data", paste0(ticker_symbol, "_marketaux_news.csv"))
  
  # Check if the news CSV file already exists
  if (file.exists(output_file_marketaux_news)) {
    cat(paste0("Existing news file found: ", output_file_marketaux_news, "\n"))
    # Read existing data, ensuring correct column types and Date format
    existing_news_df <- read_csv(output_file_marketaux_news, col_types = cols(.default = "c")) %>%
      mutate(Date = ymd_hms(Date, tz = "UTC", quiet = TRUE))
    
    # Combine existing and newly fetched news, then deduplicate the entire set
    final_news_data_df <- bind_rows(existing_news_df, news_data_combined) %>%
      distinct(UUID, .keep_all = TRUE) %>% # Deduplicate across the combined set
      arrange(desc(Date)) # Sort by date (most recent first)
    
    cat(paste0("Combined with existing news. Total unique articles now: ", nrow(final_news_data_df), "\n"))
  } else {
    cat("No existing news file found. Creating new file.\n")
    final_news_data_df <- news_data_combined %>%
      arrange(desc(Date)) # Just sort the new data
  }
  
  # Write the final, accumulated, and deduplicated news data to CSV
  write_csv(final_news_data_df, output_file_marketaux_news)
  cat(paste0("Marketaux news data (accumulated and deduplicated) saved to: ", output_file_marketaux_news, "\n"))
  
  # Crucially, update news_data_df so subsequent steps use this expanded dataset
  news_data_df <- final_news_data_df
  
} else {
  warning("No new news data was fetched in this run. Attempting to load previous news data if available.")
  output_file_marketaux_news <- file.path("data", paste0(ticker_symbol, "_marketaux_news.csv"))
  if (file.exists(output_file_marketaux_news)) {
    news_data_df <- read_csv(output_file_marketaux_news, col_types = cols(.default = "c")) %>%
      mutate(Date = ymd_hms(Date, tz = "UTC", quiet = TRUE))
    cat(paste0("Loaded ", nrow(news_data_df), " articles from existing file.\n"))
  } else {
    stop("No news data found or fetched. Cannot proceed with sentiment analysis.")
  }
}




#install.packages("TTR")

# 01_data_acquisition.R

# ... (All your previous code for Step 4 and Step 7) ...

# --- Phase 2: Data Preprocessing and Feature Engineering ---

# --- Step 8: Preparing Numerical Stock Data (Revised) ---

message("\n--- Step 8: Preparing Numerical Stock Data ---")

# Ensure 'stock_data_df' is available from previous steps
# If you are running this script in parts, you might want to load it from CSV:
# stock_data_df <- read_csv(file.path("data", paste0(ticker_symbol, "_historical_prices.csv"))) %>%
#   mutate(Date = ymd(Date)) # Ensure Date is correct type if loading from CSV

# 8.1 Calculate Daily Returns (Log Returns are often preferred for financial data)
# We'll use the 'Adjusted' close price as it accounts for splits and dividends.
# Ensure data is sorted by date for accurate lag calculations
stock_data_df <- stock_data_df %>%
  arrange(Date) %>%
  mutate(
    Log_Return = log(Adjusted / lag(Adjusted)),
    Simple_Return = (Adjusted / lag(Adjusted)) - 1
  ) %>%
  # Remove the first row which will have NA for returns
  filter(!is.na(Log_Return))

message("\nStock data with calculated returns (first 6 rows):")
print(head(stock_data_df))
message(paste("Number of rows after calculating returns:", nrow(stock_data_df))) # Check row count here

# 8.2 Calculate Technical Indicators (Examples: Moving Averages, RSI)
# Load TTR package
library(TTR)

# Convert to xts object. Include all the numerical columns we want to work with.
# The original data frame columns (Open, High, Low, Close, Volume, Adjusted, Log_Return, Simple_Return)
# are all included *before* converting to xts.
stock_data_xts_for_TTR <- xts(stock_data_df %>%
                                dplyr::select(Open, High, Low, Close, Volume, Adjusted, Log_Return, Simple_Return),
                              order.by = stock_data_df$Date)

# Calculate Simple Moving Averages (SMA)
stock_data_xts_for_TTR$SMA_10 <- SMA(stock_data_xts_for_TTR$Adjusted, n = 10)
stock_data_xts_for_TTR$SMA_50 <- SMA(stock_data_xts_for_TTR$Adjusted, n = 50)

# Calculate Relative Strength Index (RSI)
stock_data_xts_for_TTR$RSI_14 <- RSI(stock_data_xts_for_TTR$Adjusted, n = 14)

# Calculate Moving Average Convergence Divergence (MACD)
macd_data <- MACD(stock_data_xts_for_TTR$Adjusted)
stock_data_xts_for_TTR$MACD <- macd_data$macd
stock_data_xts_for_TTR$MACD_Signal <- macd_data$signal

# Convert the final xts object back to a tibble
# All original columns + new indicators are now in 'stock_data_xts_for_TTR'
processed_stock_data_df <- stock_data_xts_for_TTR %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Date") %>%
  as_tibble() %>%
  mutate(Date = ymd(Date)) %>%
  # Now, just filter out NAs introduced by indicators (e.g., first 50 rows for SMA_50)
  filter(complete.cases(.))

message("\nStock data with returns and technical indicators (first 6 rows with complete cases):")
print(head(processed_stock_data_df))
message("\nLast 6 rows (to see recent indicators):")
print(tail(processed_stock_data_df))
message(paste("Number of rows after calculating indicators and filtering NAs:", nrow(processed_stock_data_df)))

# Save the processed stock data
output_file_processed_stock <- file.path("data", paste0(ticker_symbol, "_processed_stock_data.csv"))
write_csv(processed_stock_data_df, output_file_processed_stock)
message(paste("\nProcessed stock data saved to:", output_file_processed_stock))



# 01_data_acquisition.R

# ... (All your previous code for Step 4, Step 7, and Step 8) ...

# --- Step 9: Cleaning and Preparing News Text Data ---

message("\n--- Step 9: Cleaning and Preparing News Text Data ---")

# Ensure 'news_data_df' is available from previous steps
# If running this script in parts, you might want to load it from CSV:
# news_data_df <- read_csv(file.path("data", paste0(ticker_symbol, "_marketaux_news.csv"))) %>%
#   mutate(Date = ymd_hms(Date)) # Ensure Date is correct type if loading from CSV

# Combine Headline and Snippet for a more comprehensive text for sentiment analysis
processed_news_data_df <- news_data_df %>%
  # Handle potential NA or empty strings in Headline/Snippet before combining
  mutate(
    Headline = if_else(is.na(Headline) | Headline == "", "", Headline),
    Snippet = if_else(is.na(Snippet) | Snippet == "", "", Snippet),
    Full_Text = paste(Headline, Snippet, sep = " ", collapse = NULL)
  ) %>%
  # Basic text cleaning: remove multiple spaces, leading/trailing spaces
  mutate(
    Full_Text = str_squish(Full_Text), # Removes extra whitespace and trims
    Full_Text = str_to_lower(Full_Text) # Convert to lowercase
  ) %>%
  # Select relevant columns for the next steps
  dplyr::select(Date, Headline, Snippet, Full_Text, Link, Source, Entities)

message("\nFirst few rows of processed news data with Full_Text:")
print(head(processed_news_data_df))

# Save the processed news data
output_file_processed_news <- file.path("data", paste0(ticker_symbol, "_processed_news_data.csv"))
write_csv(processed_news_data_df, output_file_processed_news)
message(paste("\nProcessed news data saved to:", output_file_processed_news))




# 01_data_acquisition.R

# ... (All your previous code for Step 4, Step 7, Step 8, and Step 9) ...

# --- Phase 3: Sentiment Analysis and Integration ---

# --- Step 10: Performing Sentiment Analysis on News Text ---

message("\n--- Step 10: Performing Sentiment Analysis on News Text ---")

# Load necessary packages for text analysis
library(tidytext)    # For text mining in a 'tidyverse' way
library(textdata)    # To easily access sentiment lexicons
library(stringr)     # For string manipulation (already loaded, but good to ensure)
library(dplyr)       # For data manipulation (already loaded, but good to ensure)
library(lubridate)   # For date handling (already loaded, but good to ensure)

# Ensure 'processed_news_data_df' is available
# If running this script in parts, load it from CSV:
# processed_news_data_df <- read_csv(file.path("data", paste0(ticker_symbol, "_processed_news_data.csv"))) %>%
#   mutate(Date = ymd_hms(Date)) # Ensure Date is correct type if loading from CSV

# Get the Loughran-McDonald financial sentiment lexicon
# This might take a moment the first time it downloads
message("Downloading Loughran-McDonald sentiment lexicon...")
data("stop_words") # Standard English stop words
loughran_lexicon <- get_sentiments("loughran")

# We'll assign positive/negative/etc. to words based on the lexicon
# Convert the sentiment lexicon to have simple positive/negative scores for now.
# Loughran-McDonald has 'positive', 'negative', 'litigious', 'uncertainty', 'constraining', 'superfluous'
# For a simple score, we'll focus on positive and negative.
positive_words <- loughran_lexicon %>% filter(sentiment == "positive") %>% pull(word)
negative_words <- loughran_lexicon %>% filter(sentiment == "negative") %>% pull(word)

# Tokenize the news text (break into individual words)
# Then join with the sentiment lexicon to get a sentiment score for each word.
# Group by unique article (using a row_number() as a temporary ID if no unique ID)
# Marketaux 'uuid' can be used if you included it, otherwise row_number() is fine.
# Let's add a unique ID to processed_news_data_df for safe joining after tokenization
processed_news_data_df <- processed_news_data_df %>%
  mutate(article_id = row_number()) # Create a unique ID for each article

article_sentiment <- processed_news_data_df %>%
  unnest_tokens(word, Full_Text) %>% # Break Full_Text into individual words
  anti_join(stop_words, by = "word") %>% # Remove common stop words (e.g., "the", "a", "is")
  # Filter out numbers and punctuation if any remain
  filter(str_detect(word, "[a-z]")) %>% # Keep only words containing alphabetic characters
  # Assign sentiment score: +1 for positive words, -1 for negative words
  mutate(
    sentiment_score = case_when(
      word %in% positive_words ~ 1,
      word %in% negative_words ~ -1,
      TRUE ~ 0 # Neutral words (not in positive/negative lists) get 0
    )
  ) %>%
  # Aggregate sentiment per article
  group_by(article_id, Date, Headline, Snippet, Link, Source, Entities) %>%
  summarise(
    Article_Sentiment_Score = sum(sentiment_score, na.rm = TRUE), # Sum of word scores
    Number_of_Sentiment_Words = sum(sentiment_score != 0, na.rm = TRUE), # Count words contributing to sentiment
    .groups = "drop" # Drop grouping to get a clean data frame
  ) %>%
  # Calculate average sentiment score, or normalize by number of words if desired
  mutate(
    # Avoid division by zero if no sentiment words found
    Average_Article_Sentiment = if_else(Number_of_Sentiment_Words > 0,
                                        Article_Sentiment_Score / Number_of_Sentiment_Words,
                                        0)
  )

message("\nFirst few articles with calculated sentiment:")
print(head(article_sentiment))
message(paste("Number of articles with sentiment scores:", nrow(article_sentiment)))

# Save the article-level sentiment data
output_file_article_sentiment <- file.path("data", paste0(ticker_symbol, "_article_sentiment.csv"))
write_csv(article_sentiment, output_file_article_sentiment)
message(paste("\nArticle-level sentiment data saved to:", output_file_article_sentiment))


# 01_data_acquisition.R

# ... (All your previous code up to Step 10) ...

# --- Step 11: Aggregate Sentiment to Daily Level ---

message("\n--- Step 11: Aggregate Sentiment to Daily Level ---")

# Ensure 'article_sentiment' is available
# If running this script in parts, load it from CSV:
# article_sentiment <- read_csv(file.path("data", paste0(ticker_symbol, "_article_sentiment.csv"))) %>%
#   mutate(Date = ymd_hms(Date)) # Ensure Date is correct type if loading from CSV

daily_sentiment_df <- article_sentiment %>%
  # Extract just the date part for grouping
  mutate(Trading_Date = as_date(Date)) %>%
  group_by(Trading_Date) %>%
  summarise(
    Daily_Avg_Sentiment = mean(Average_Article_Sentiment, na.rm = TRUE),
    Daily_Sum_Sentiment = sum(Article_Sentiment_Score, na.rm = TRUE),
    Num_Articles_That_Day = n(),
    .groups = "drop"
  ) %>%
  arrange(Trading_Date)

message("\nDaily aggregated sentiment data:")
print(daily_sentiment_df)

# Save the daily aggregated sentiment data
output_file_daily_sentiment <- file.path("data", paste0(ticker_symbol, "_daily_sentiment.csv"))
write_csv(daily_sentiment_df, output_file_daily_sentiment)
message(paste("\nDaily aggregated sentiment data saved to:", output_file_daily_sentiment))


# 01_data_acquisition.R

# ... (All your previous code up to Step 11) ...

# --- Step 12: Integrate Sentiment Data with Processed Stock Data ---

message("\n--- Step 12: Integrate Sentiment Data with Processed Stock Data ---")

# Ensure 'processed_stock_data_df' and 'daily_sentiment_df' are available
# If running this script in parts, load them from CSV:
# processed_stock_data_df <- read_csv(file.path("data", paste0(ticker_symbol, "_processed_stock_data.csv"))) %>%
#   mutate(Date = ymd(Date))
# daily_sentiment_df <- read_csv(file.path("data", paste0(ticker_symbol, "_daily_sentiment.csv"))) %>%
#   mutate(Trading_Date = ymd(Trading_Date))

# Rename 'Date' in stock data to 'Trading_Date' to match sentiment for joining
processed_stock_data_df_renamed <- processed_stock_data_df %>%
  rename(Trading_Date = Date)

# Perform a left join to add sentiment data to stock data
# A left join keeps all stock dates and adds sentiment if available for that date.
# If a stock date has no news, the sentiment columns will be NA.
integrated_data_df <- processed_stock_data_df_renamed %>%
  left_join(daily_sentiment_df, by = "Trading_Date") %>%
  # Handle NA sentiment values if no news for a day (e.g., fill with 0 or previous day's value)
  # For now, let's fill NAs with 0 for sentiment scores, assuming no news means neutral impact.
  mutate(
    Daily_Avg_Sentiment = replace_na(Daily_Avg_Sentiment, 0),
    Daily_Sum_Sentiment = replace_na(Daily_Sum_Sentiment, 0),
    Num_Articles_That_Day = replace_na(Num_Articles_That_Day, 0)
  ) %>%
  arrange(Trading_Date) # Ensure final dataset is sorted by date

message("\nFirst few rows of integrated stock and sentiment data:")
print(head(integrated_data_df))
message("\nLast few rows of integrated stock and sentiment data:")
print(tail(integrated_data_df))
message(paste("Number of rows in integrated data:", nrow(integrated_data_df)))


# Save the final integrated dataset
output_file_integrated_data <- file.path("data", paste0(ticker_symbol, "_integrated_financial_data.csv"))
write_csv(integrated_data_df, output_file_integrated_data)
message(paste("\nIntegrated financial data saved to:", output_file_integrated_data))

message("\n--- Data Acquisition and Preprocessing COMPLETE! ---")
