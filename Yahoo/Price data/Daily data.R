lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

prices.yahoo <- function(y, s = NULL, e = NULL){ # Stock Price Data from Yahoo
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  for (A in y){ if (is.null(s) && is.null(e)) { 
    
      q <- getSymbols(A, src = "yahoo", auto.assign = F)
    
    } else if (is.null(e)){ q <- getSymbols(A,from=s,src="yahoo",auto.assign=F)
  
    } else if (is.null(s)){ q <- getSymbols(A, to=e, src="yahoo",auto.assign=F)
  
    } else { q <- getSymbols(A, from = s, to = e, src="yahoo", auto.assign=F) }
    
    p <- cbind(p, q[,4]) } # Join all columns into one data frame
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in column names
  
  as.timeSeries(p) # Make it time series and display
}
prices.yahoo(y = c("UNM", "AIG", "HIG"), s = "2022-01-01") # Test
