lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

prices.yahoo.year <- function(x, y){ # Get stock price data for certain year
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction 
  for (A in x){ p <- cbind(p, getSymbols(A, from = sprintf("%s-01-01", y),
                                         to = sprintf("%s-12-31", y),
                                         src = "yahoo", auto.assign = F)[,4]) }

  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in data set
  
  as.timeSeries(p) # Make it time series
}
prices.yahoo.year(c("AAPL", "MSFT", "GE"), 2023) # Test
