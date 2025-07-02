library(tidyquant)
library(reticulate)
library(tidyverse)
library(bizdays)


# Function to get a date that is n trading days before a given date
# Using the timeDate package to get holidays
get_past_trading_day <- function(from_date = Sys.Date(), n_days_back = 1) {
  # Check if required packages are installed
  if (!requireNamespace("timeDate", quietly = TRUE)) {
    stop("Package 'timeDate' is needed for this function. Please install it with: install.packages('timeDate')")
  }
  
  # Convert input date to Date object if it's not already
  from_date <- as.Date(from_date)
  
  # Get NYSE holidays for the relevant years
  years <- as.numeric(format(seq(from_date - 365*2, from_date, by = "day"), "%Y"))
  years <- unique(years)
  
  # Get all NYSE holidays for these years
  holidays <- NULL
  for (year in years) {
    nyse_holidays <- timeDate::holidayNYSE(year)
    nyse_holidays = as.Date(nyse_holidays)
    holidays <- c(holidays, nyse_holidays)
  }
  
  # Counter for trading days
  days_counted <- 0
  current_date <- from_date
  
  # Go backward until we've found n trading days
  while(days_counted < n_days_back) {
    # Move one calendar day back
    current_date <- current_date - 1
    
    # Check if it's a weekday and not a holiday
    is_not_weekday <- !weekdays(current_date) %in% c("Saturday", "Sunday")
    is_not_holiday <- !current_date %in% nyse_holidays
    
    # If it's a trading day, increment our counter
    if(is_not_weekday && is_not_holiday) {
      days_counted <- days_counted + 1
    }
  }
  
  return(current_date)
}

# Example usage:
# Get date that is 5 trading days before today
# trading_date <- get_past_trading_day(Sys.Date(), 5)
# print(trading_date)


load_python = function () {
  py_run_string("
import yfinance as yf
import numpy as np

def get_call_exp_date(ticker='^SPX'):
    ticker = ticker.upper()
    stock = yf.Ticker(ticker)
    exp_dates = stock.options
    return exp_dates

def get_calls(ticker='^SPX', exp_index=20):
    exp_index = int(exp_index)
    ticker = ticker.upper()
    stock = yf.Ticker(ticker)
    exp_dates = stock.options
    exp_date_month = exp_dates[exp_index]
    opt_chain = stock.option_chain(exp_date_month).calls
    return opt_chain
")
}




expected_call = function (symbol, opt_chain, expDate_month, make_mean_0 = FALSE) {
  
  #creates calendar for the bizdays to calculate days till experation
  holidays <- timeDate::holidayNYSE(2025)
  create.calendar("NYSE", holidays = as.Date(holidays), weekdays = c("saturday", "sunday"))
  # Define the stock symbol and get the last month's daily returns
  # Replace with your desired stock symbol
  n_days = bizdays(Sys.Date(), ymd(expDate_month), "NYSE") #find days till expiration
  last_month <- get_past_trading_day(n_days_back = n_days) #days till expiration in the past
  
  stock = tq_get(symbol, from = last_month, to = Sys.Date(), get = "stock.prices")
  returns = stock %>%
    tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "arithmetic") %>% 
    mutate(daily.returns = 1+daily.returns) %>% 
    drop_na()
  
  
  #makes the paramaters depending on if you want to purly estimate them from data
  if (make_mean_0) {
    sigma_for_n_days = sqrt(sum((log(returns$daily.returns)-0)^2)/(n_days-1))*sqrt(n_days)
    mu = 0
  } else {
    mu = mean(log(returns$daily.returns))
    sigma_for_n_days = sqrt(sum((log(returns$daily.returns)-mu)^2)/(n_days-1))*sqrt(n_days)
  }
  
  
  
  strike_price = opt_chain$strike
  last_price <- as.numeric( # get the last adjusted price
    stock %>%
      arrange(desc(date)) %>%
      slice_head(n = 1) %>%
      select(adjusted)
  )
  strike_return = strike_price/last_price # turns the strike price into a rate of return so the lognormal can use it
  
  partial_expectation = exp(mu+sigma_for_n_days^2/2)*pnorm((mu+sigma_for_n_days^2-log(strike_return))/sigma_for_n_days, 0, 1) #expectation of itm
  
  otm_probability = plnorm(strike_return, mu, sigma_for_n_days) #cdf of the lognormal from 0 to strike price
  itm_probability = 1-otm_probability
  
  expected_gain = (partial_expectation * last_price - strike_price * itm_probability - opt_chain$lastPrice)*100# no otm, because otm gain is 0, partial_expectation already has probability applied to itself
  
  final_dataframe = opt_chain %>%
    mutate(expected_gain = expected_gain, itm_prob = itm_probability, days_till_exp = n_days) %>% #combine out expected gains with the options dataframe
    select(itm_prob, expected_gain, days_till_exp, everything()) %>% 
    mutate(lastTradeDate = format(lastTradeDate, "%Y-%m-%d %H:%M:%S"))
  
  return(final_dataframe)
}