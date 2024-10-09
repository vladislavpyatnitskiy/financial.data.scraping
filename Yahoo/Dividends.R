lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

s.dividends <- function(x, s = NULL, e = NULL){ # Dividends data
  
  d <- NULL # Create an empty variables
  no.d <- NULL # list of companies not paying dividends for the period
  
  for (A in x){ if (is.null(s) && is.null(e)) { # When no start or end date
    
    if (is.null(d)){ d <- getDividends(A) # Get dividends or add to new list
    
    if (isTRUE(length(d) == 0)){ no.d <- c(no.d, A) } else {
      
      r.names <- rownames(d) # Subtract dates
      
      d <- cbind(r.names, d) } } else { d2 <- getDividends(A)
      
      if (isTRUE(length(d2) == 0)){ no.d <- c(no.d, A) } else {
        
        r.names2 <- rownames(d2) # Subtract dates
        
        d2 <- cbind(r.names2, d2) # Join
        
        d <- merge(d, d2, all = T) } } # When there is start date:
    
  } else if (is.null(e)) { if (is.null(d)){ d <- getDividends(A, from = s)
  
  if (isTRUE(length(d) == 0)){ no.d <- c(no.d, A) } else {
    
    r.names <- rownames(d) # Subtract dates
    
    d <- cbind(r.names, d) } } else { d2 <- getDividends(A, from = s)
    
    if (isTRUE(length(d2) == 0)){ no.d <- c(no.d, A) } else {
      
      r.names2 <- rownames(d2) # Subtract dates
      
      d2 <- cbind(r.names2, d2) # Join
      
      d <- merge(d, d2, all = T) } } # When there is end date:
    
  } else if (is.null(s)) { if (is.null(d)){ d <- getDividends(A, to = e)
  
  if (isTRUE(length(d) == 0)){ no.d <- c(no.d, A) } else {
    
    r.names <- rownames(d) # Subtract dates
    
    d <- cbind(r.names, d) } } else { d2 <- getDividends(A, to = e)
    
    if (isTRUE(length(d2) == 0)){ no.d <- c(no.d, A) } else {
      
      r.names2 <- rownames(d2) # Subtract dates
      
      d2 <- cbind(r.names2, d2) # Join
      
      d<-merge(d, d2, all=T) } } # When there are both start and end dates:
    
  } else { if (is.null(d)){ d <- getDividends(A, from = s, to = e)
  
  if (isTRUE(length(d) == 0)){ no.d <- c(no.d, A) } else {
    
    r.names <- rownames(d) # Subtract dates
    
    d <- cbind(r.names, d) } } else { d2 <- getDividends(A, from = s, to = e)
    
    if (isTRUE(length(d2) == 0)){ no.d <- c(no.d, A) } else {
      
      r.names2 <- rownames(d2) # Subtract dates
      
      d2 <- cbind(r.names2, d2) # Join
      
      d <- merge(d, d2, all = T) } } } } # End of data collection
  
  d[is.na(d)] <- 0 # Substitute NA with 0
  
  k <- colnames(d)
  
  d <- as.timeSeries(d) # Make them Time Series
  
  r.names3 <- as.data.frame(rownames(d)) # Transform data into Data Frame
  
  r.names5 <- NULL # Create Data Frame for Stocks without dividends
  
  for (n in 1:length(no.d)){ r.names4 <- data.frame(r.names3, 0)
  
    colnames(r.names4) <- c("Date", no.d[n])
    
    if (is.null(r.names5)){ r.names5 <- r.names4 } else {
      
      r.names5 <- merge(r.names5, r.names4, by = "Date") } }
    
  rownames(r.names5) <- r.names5[,1] # Put dates into row names
  
  r.names5 <- r.names5[,-1] # Reduce dates column
  
  d <- cbind(d, as.timeSeries(r.names5)) # Join
  
  k <- c(k, no.d)
  
  colnames(d) <- k
  
  l <- NULL # Create empty list to clean column names
  
  for (n in 1:length(colnames(d))){
    
    if (isTRUE(grepl("div", colnames(d)[n], fixed = T))){ 
      
      l <- c(l, gsub('[".div"]','', colnames(d)[n]))
      
      } else { l <- c(l, colnames(d)[n]) } } #
  
  colnames(d) <- l # Put clean names to Data Frame
  
  d <- d[,x] # Change order of columns
  
  d # Display
}
s.dividends(c("AAPL","MSFT","AMZN","C","GOOGL","VSTO"),s="2022-07-19") # Test
