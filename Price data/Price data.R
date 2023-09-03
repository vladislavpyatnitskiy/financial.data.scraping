# Access necessary libraries
lapply(c("quantmod", "timeSeries"), require, character.only = TRUE)
# Type tickers
tickers <- c("AMZN", "GOOGL", "C")

# Type start date
start_date <- "2020-02-20"

# Type end date
end_date <- NULL

# Set up function
prices_from_yahoo <- function(y, z = NULL, i = NULL){
  # Create an empty variable
  portfolioPrices <- NULL
  # Loop for data extraction
  for (Ticker in y){
    # Set up statements for start and end dates
    if (is.null(z) && is.null(i)) {
      # When neither start date nor end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker, src = "yahoo",
                                          auto.assign=FALSE)[,4])
      } else if (is.null(i)) {
      # When only start date is defined
      portfolioPrices <- cbind(portfolioPrices,
                                getSymbols(Ticker, from = z, src = "yahoo",
                                          auto.assign=FALSE)[,4])
      } else if (is.null(z)) {
      # When only end date is defined
      portfolioPrices <- cbind(portfolioPrices,
                                 getSymbols(Ticker, to = i, src = "yahoo",
                                          auto.assign=FALSE)[,4])
      } else { 
      # When both start date and end date are defined
      portfolioPrices <- cbind(portfolioPrices,
                                 getSymbols(Ticker, from = z, to = i,
                                          src = "yahoo", 
                                          auto.assign=FALSE)[,4])
    }
  }
  # Get rid of NAs
  portfolioPrices <- portfolioPrices[apply(portfolioPrices,1,
                                           function(x) all(!is.na(x))),]
  # Put the tickers in data set
  colnames(portfolioPrices) <- y
  
  # Make data discrete
  portfolioReturns <- ROC(portfolioPrices, type = "discrete")
  
  # Make it time series
  portfolioReturns <-as.timeSeries(portfolioPrices)
  
  # Show output
  return(portfolioReturns)
}

stock_data <- prices_from_yahoo(tickers, start_date, end_date)
head(stock_data)
