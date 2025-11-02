# MTH208_Course_Project
##Financial Dashboard (MTH Group Project)
This Shiny application provides a multi-tab dashboard for financial analysis. It combines technical stock analysis, Markowitz portfolio optimization, and option chain analysis with Black-Scholes (BSM) modeling, all presented in a custom gold-and-dark theme.

📈 Features
Technical Analysis: View interactive candlestick charts (OHLCV) with volume. Apply common technical indicators like:

Simple Moving Average (SMA)

Exponential Moving Average (EMA)

Bollinger Bands

Relative Strength Index (RSI)

Moving Average Convergence Divergence (MACD)

Portfolio Optimization: Input a list of stock tickers to calculate and visualize the Markowitz efficient frontier. It also displays the calculated weights for the global minimum variance portfolio.

Option Chain Data: Fetch and display the current call and put option chains for a specified ticker.

Black-Scholes Analysis: Fetches an option chain and compares the market price (Last, Bid, Ask) against a theoretical BSM price. It also generates a plot of the Implied Volatility (IV) vs. Strike price (volatility smile/skew).

🛠️ Prerequisites
This application requires R and several packages.

Package Dependencies
The following R packages are necessary to run the app:

shiny

shinydashboard

bslib

quantmod

TTR

DT

xts

zoo

yfR

PerformanceAnalytics

quadprog

ggplot2

Installation
You can install all required packages by running the following command in your R console.
install.packages(c(
  "shiny", "shinydashboard", "bslib", "quantmod", "TTR",
  "DT", "xts", "zoo", "yfR", "PerformanceAnalytics",
  "quadprog", "ggplot2"
))
Save the File: Save the provided code as app.R in a new directory.

Open R/RStudio: Launch your R environment (RStudio is recommended).
shiny::runApp("app.R")Data Sources (Data Scraping)
This application fetches live financial data directly from public sources and does not rely on cached or static data files.

Stock Data (OHLCV): Fetched from Yahoo Finance using the yfR::yf_get() function. The code explicitly uses do_cache = FALSE to ensure the most recent data is retrieved on each request.

Option Chains & Quotes: Fetched from Yahoo Finance using the quantmod::getOptionChain() and quantmod::getQuote() functions.
Note: As the app depends on live web-scraped data, its functionality is subject to changes in the Yahoo Finance API or potential connection issues.
