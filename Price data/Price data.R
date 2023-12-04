lapply(c("quantmod", "timeSeries"), require, character.only = T) # libraries

prices.yahoo <- function(y, s = NULL, e = NULL){
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction & # Set up statements for start and end dates
  for (Ticker in y){ if (is.null(s) && is.null(e)) {
    
      # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(Ticker, src = "yahoo", auto.assign=F)[,4])
      
      } else if (is.null(e)) { # When only start date is defined
      
      p <- cbind(p, getSymbols(Ticker, from = s,src="yahoo",auto.assign=F)[,4])
      
      } else if (is.null(s)) { # When only end date is defined
      
      p <- cbind(p,getSymbols(Ticker,to=e,src="yahoo",auto.assign=F)[,4])
      
      } else { # When both start date and end date are defined
      
      p<-cbind(p,getSymbols(Ticker,from=s,to=e,src="yahoo",auto.assign=F)[,4])}
  }
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA

  colnames(p) <- y # Put the tickers in data set
  
  r <- as.timeSeries(p) # Make it time series
  
  return(r) # Show output
}
prices.yahoo(y = "UNM", s = "2022-01-01")
