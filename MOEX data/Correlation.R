lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

moex.correlation <- function(x){ # Correlation coefficients for Russian stocks
  
  p <- NULL # Create an empty variable and get stock price data
  
  for (a in x){ 
    
    D <- as.data.frame(get_candles(a, "2007-01-01", till = as.Date(Sys.Date()),
                                   interval = 'daily')[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- x
  
  correlation = diff(log(as.timeSeries(p)))[-1,] # returns matrix 
  
  cor(correlation)  
}
moex.correlation(c("LKOH", "SBER", "MGNT"))
