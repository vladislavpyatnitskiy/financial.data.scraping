lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

prices.yahoo <- function(y, s = NULL, e = NULL){ # Stock Price Data from Yahoo
  
  p <- NULL # There are 4 conditions for getting data:
  
  for (A in y){ if (is.null(s) && is.null(e)) { # When Time Period not defined
    
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
      p <- cbind(p,getSymbols(A, from = s, src = "yahoo", auto.assign = F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p, getSymbols(A, to = e, src = "yahoo", auto.assign = F)[,4])
    
    } else { # When both start date and end date are defined
      
      p <- cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in column names
  
  as.timeSeries(p) # Make it time series and display
}
prices.yahoo(y = "UNM", s = "2022-01-01") # Test
