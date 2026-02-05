lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

moex.correlation <- function(x){ # Correlation coefficients for Russian stocks
  
  p <- NULL # Create an empty variable and get stock price data
  l <- NULL # to store start and end dates for available trading days
  
  for (a in x){ 
    
    D = as.data.frame(get_candles(a,"2007-01-01",interval='daily')[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    D <- D[apply(D, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(D) <- x[n] # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (x[n] == "BELU"){ f <- which(rownames(D) == "2024-08-15")
    
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    message(
      sprintf(
        "%s is downloaded (%s/%s)", 
        a, which(x == a), length(x)
      )
    )
    l <- rbind.data.frame(l, cbind(rownames(D)[1], rownames(D)[nrow(D)]))
    
    p <- cbind(p, D) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NA
  
  colnames(p) <- x
  
  colnames(l) <- c("Start Date", "End Date")
  rownames(l) <- x
  
  L <- list(
    cor(diff(log(as.timeSeries(p)))[-1,]),
    sprintf(
      "From %s to %s.", 
      rownames(p)[1],
      rownames(p)[nrow(p)]
      ),
    l
    ) # returns matrix 
  
  names(L) <- c(
    "Correlation", 
    "Time Period for all Securities", 
    "Time Period for each security"
    )
  
  L
}
moex.correlation(c("LKOH", "SBER", "MGNT", "BELU"))
