# Access necessary libraries
lapply(c("quantmod", "timeSeries"),
       require, character.only = TRUE)

# Type tickers
tickers <- c("AMZN", "GOOGL")

# Type start date
start_date <- "2020-02-20"

# Set up function
prices_from_yahoo <- function(y, z){
  # Create an empty variable
  portfolioPrices <- NULL
  
  # Loop for data extraction
  for (Ticker in y){
    # fill the variable with dataset
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols(Ticker, from = z, src = "yahoo",
                                        auto.assign=FALSE)[,4])
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

# Type variables into function
prices_from_yahoo(tickers, start_date)
