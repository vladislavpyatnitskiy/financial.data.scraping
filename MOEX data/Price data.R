lapply(c("moexer", "xts", "timeSeries"), require, character.only = T) # Libs

prices.moex <- function(x, s = NULL, e = NULL, split = F, all=F){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  getData <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    colnames(D) <- A # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (A == "BELU" & isTRUE(split) &
        (isTRUE(s < "2024-08-15" | e < "2024-08-15") |
         isTRUE(is.null(s) | is.null(e)))){
      
      f <- which(rownames(D) == "2024-08-15")
    
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    p <- cbind(p, D) } # Merge
    
  if (!all) p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  as.timeSeries(p) # Display time series
}
prices.moex(c("ROSN", "BELU"), s = "2024-07-01", split = T, all = T) # Test
