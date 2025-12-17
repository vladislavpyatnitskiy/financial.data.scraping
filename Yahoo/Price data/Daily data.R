lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

prices.yahoo <- function(y, s = NULL, e = NULL){ # Stock Price Data from Yahoo
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in y){ p <- cbind(p, getData(A, s, e)[,4]) 
  
    message(sprintf("%s is downloaded; %s from %s", A, which(y==A), length(y)))
  
  } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in column names
  
  as.timeSeries(p) # Make it time series and display
}
prices.yahoo(y = c("UNM", "AIG", "HIG"), s = "2022-01-01") # Test
