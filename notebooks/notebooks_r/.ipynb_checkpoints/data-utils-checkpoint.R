# Data Utilities
# ==============

# These utilites require you to have a variable in your global environment correpsonding to the location of your FXBootcamp folder
#
# For example...
# DATA_FOLDER <- 'C:/RWData/FXBootcamp/'

# TODO - Document & test function behaviour
# TODO - Faciltiy to convert currencies in list to have same quote currency 
# TODO - Construct strength indexes as per @integracore
# TODO - Total return indexes (including return from interest rate differential)
# TODO - Handle things that aren't FX pairs


library(tidyquant, quietly = TRUE)


get_asset_list <- function(x) {
  read.csv(file.path(DATA_FOLDER, 'Zorro-Assets-Lists', paste0(x, '.csv')), stringsAsFactors = FALSE)
}

get_daily_OHLC_ticker <- function(ticker) {
  td <- data.frame(matrix(ncol=7, nrow=0))
  try(td <- cbind(Ticker = ticker, read.csv(file.path(DATA_FOLDER, 'Daily', paste0(ticker,'.csv')), header = FALSE, stringsAsFactors = FALSE)))
  colnames(td) <- c('Ticker', 'Date', 'Open' ,'High', 'Low', 'Close', 'Volume')
  td$Date <- as.Date(as.character(td$Date), '%Y%m%d')
  td$Ticker <- as.character(td$Ticker)
  return(td)
}

get_hourly_OHLC_ticker <- function(ticker) {
  td <- data.frame(matrix(ncol=8, nrow=0))
  try(td <- cbind(Ticker = ticker, read.csv(file.path(DATA_FOLDER, 'Hourly', paste0(ticker,'.csv')), header = FALSE, stringsAsFactors = FALSE)))
  colnames(td) <- c('Ticker', 'Date', 'Time', 'Open' ,'High', 'Low', 'Close', 'Volume')
  td$Date <- as.Date(as.character(td$Date), '%Y%m%d')
  td$Ticker <- as.character(td$Ticker)
  return(td)
}

get_daily_OHLC <- function(tickers) {
  tickers <- gsub('/', '', tickers) # filenames do not have /'s but Zorro assetlists do  
  l <- lapply(tickers, get_daily_OHLC_ticker)
  bind_rows(l)
}

get_hourly_OHLC <- function(tickers) {
  tickers <- gsub('/', '', tickers) # filenames do not have /'s but Zorro assetlists do  
  l <- lapply(tickers, get_hourly_OHLC_ticker)
  bind_rows(l)
}

# Get all FX pairs containing a given currency, and convert exchange rates (and switch the ticker) such
# that specified currency is always the quote currency of the pair.
# Anything that is not FX or for which the currency does not appear in either the base or quote will be discarded.
convert_common_quote_currency <- function(prices_df, quote_currency) {
  
 # Convert all records where the Base currency is the required quote currency and take the reciprocal of prices
 prices_df %>%
    mutate(Base = str_sub(Ticker, 1, 3)) %>%
    mutate(Quote = str_sub(Ticker, -3)) %>%
    filter(Base == quote_currency & Quote != quote_currency) %>%
    # Take reciprocal of prices
    mutate(Ticker = paste0(Quote,Base),
           Open = 1/Open,
           NewHigh = 1/Low,
           Low = 1/High,
           High = NewHigh,
           Close = 1/Close) %>%
    select(c(-Base, -Quote, -NewHigh)) %>%
    # Append all records where the Quote currency is the required quote currency
    bind_rows(
      prices_df %>%
        mutate(Base = str_sub(Ticker, 1, 3)) %>%
        mutate(Quote = str_sub(Ticker, -3)) %>%
        filter(Base != quote_currency & Quote == quote_currency) %>%
        select(c(-Base, -Quote))
    )

}

get_policy_rates <- function(currencies) {
  l <- lapply(currencies, function(currency) {
    td <- data.frame(matrix(ncol=3, nrow=0))
    try(td <- cbind(Currency = currency, read.csv(file.path(DATA_FOLDER, paste0(currency,'.csv')), header = TRUE, stringsAsFactors = FALSE)))
    colnames(td) <- c('Currency', 'Date', 'Rate')
    td$Date <- as.Date(as.character(td$Date), '%Y-%m-%d')
    td$Currency <- as.character(td$Currency)
    td %>% 
      na.omit() %>%
      arrange(Date)
  })
  bind_rows(l)
}

# Pass prices and policy rates data frames
# Function will append interest accrual information and an interest index
append_interest_rate_differential <- function(prices_df, policy_rates_df) {

  extended_prices <- prices_df %>%
    # 1. split the ticker into Base and Quote currencies
    mutate(Base = str_sub(Ticker, 1,3),
           Quote = str_sub(Ticker, -3)) %>%
    left_join(policy_rates_df, by = c('Base' = 'Currency', 'Date' = 'Date')) %>%
    left_join(policy_rates_df, by = c('Quote' = 'Currency', 'Date' = 'Date')) %>%
    # Carry NAs at the end of the series forward by ticker
    group_by(Ticker) %>%
    arrange(Ticker, Date) %>%
    mutate(
      Base_Rate = na.locf(Rate.x, na.rm = FALSE)*0.01, # Leave leading NAs but carry everything else forward
      Quote_Rate = na.locf(Rate.y, na.rm = FALSE)*0.01,
      Rate_Diff = Base_Rate - Quote_Rate,
      Daycount_Fraction = (as.numeric(Date) - as.numeric(lag(Date))) / 365,
      # 5. Calcualte interest returns, the daycount fraction * the rate differential
      Interest_Returns = Daycount_Fraction * Rate_Diff,
      # 6. Calculate Interest_Accrual_on_Spot, the interest that accrues since the last observation on a single unit of currency given the last spot rate, expressed in the quote currency. 
      # This is Daycount_Fraction * Rate_Diff * closing exchange rate
      Interest_Accrual_on_Spot = Daycount_Fraction * Rate_Diff * Close, 
      # 7. Calculate Spot returns from the closing prices
      Spot_Returns = Close / lag(Close) - 1
    ) %>% 
    # 8. Remove records for which we don't have interest rates (after carrying forward)
    select(c(-Rate.x, -Rate.y)) %>% # drop raw rates before omit as they might still contain NAs, hence leading to loss of rows that were filled by na.locf
    na.omit

  
  # 9. Calculate total return indexes which assumes periodic compounding of interest (on each price observation)
  extended_prices %>%
    group_by(Ticker) %>%
    mutate(
      Spot_Return_Index = cumprod(1 + Spot_Returns), # Not necessary, but useful for validation
      Interest_Return_Index = cumprod(1 + Interest_Returns),
      Total_Return_Index = cumprod(1+ Spot_Returns + Interest_Returns)
    )

}

# Get unique currencies from a data frame of price data
get_unique_currencies <- function(prices_df) {
  base_tickers <- prices_df %>%
    mutate(Base = str_sub(Ticker, 1, 3)) %>%
    distinct(Base)
  
  quote_tickers <- prices_df %>%
    mutate(Quote = str_sub(Ticker, -3)) %>%
    distinct(Quote)
  
  as.vector(unique(c(base_tickers[[1]],quote_tickers[[1]])))
}

