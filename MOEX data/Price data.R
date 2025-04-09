lapply(c("moexer", "xts", "timeSeries"), require, character.only = T) # Libs

prices.moex <- function(x, s = NULL, e = NULL){
  
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
    
    p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Assign column names
  
  as.timeSeries(p) # Display time series
}
prices.moex(c("RTSI", "SBER", "ROSN")) # Test
