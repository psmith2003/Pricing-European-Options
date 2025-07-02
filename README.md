# Pricing-European-Options
Senior Project: Using probability theory to price options

- Uses tidyquant in R to gather historical data on a security, then retrieves options data using yfinance in Python.
- Historical data is used to estimate a probability distribution's parameters.
- This distribution is used to calculate the probability of profit or loss for each option.
- Option prices are estimated based on these probabilities and the strike prices.
- These estimated prices are compared to current market prices to identify potentially mispriced options.
