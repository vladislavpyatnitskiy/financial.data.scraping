# Web Scraping of Financial Data

[![R](https://img.shields.io/badge/R-4.x-blue.svg)](https://www.r-project.org/)
![GitHub last commit](https://img.shields.io/github/last-commit/vladislavpyatnitskiy/financial.data.scraping.svg)

| | AAPL | MSFT | GOOGL | AMZN | META |
|---|---|---|---|---|---|
Market Cap (intraday) | 2.95T | 2.75T | 1.70T | 1.50T | 861.01B |
Enterprise Value | 3.00T | 2.69T | 1.61T | 1.57T | 836.10B |
Trailing P/E | 30.94 | 35.84 | 25.92 | 76.01 | 29.57 |
Forward P/E | 28.82 | 33.56 | 20.28 | 43.29 | 19.92
PEG Ratio (5 yr expected) | 2.24 | 2.24 | 1.27 | 2.70 | 0.77 |
Price/Sales (ttm) | 7.83 | 12.65 | 5.83 | 2.72 | 6.94 |
Price/Book (mra) | 47.47 | 12.45 | 6.20 | 8.20 | 6.03 |
Enterprise Value/Revenue | 7.83 | 12.32 | 5.43 | 2.84 | 6.59 |
Enterprise Value/EBITDA | 73.27 | 74.01 | 17.27 | 21.37 | 17.60

#### Table. 1. Example of scraped data from security's statistics tab of Yahoo! Finance
--------------
Welcome to the Financial Data Scraping repository! This repository provides a convenient tool for fetching financial data from various sources including Yahoo Finance, Investing.com, SmartLab, and Finviz. Whether you're a data analyst, investor, or researcher, this tool simplifies the process of gathering crucial financial information for your analysis and decision-making.

FYI: US version of Yahoo! Finance is temporarily for parsing, add uk. (e.g. "https://uk.finance.yahoo.com/quote/%s/profile")

## Features:

• Data from Multiple Sources: Access financial data from Yahoo Finance, Investing.com, SmartLab, and Finviz, allowing for comprehensive analysis and comparison.

• Easy-to-Use Interface: Simple and intuitive functions make it easy to retrieve data with just a few lines of code.

• Customizable Parameters: Customize your data requests with parameters tailored to your specific needs, such as date range, stock symbols, and more.

## Libraries

• quantmod

• rvest
