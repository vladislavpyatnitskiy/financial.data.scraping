# Use Data Frame from function enabling to get data from Smartlab website
smartlab.screener <- function(x){ # Screener for stocks
  
  for (n in 1:ncol(x)){ x[,n] <- as.numeric(x[,n]) } # Make data numeric
  
  # Screener settings
  sm.model1 <- x[x$`EV/EBITDA` < 10 & x$`DEBT/EBITDA` < 4 & x$`P/E` < 20 &
                   x$`P/BV` < 3 & x$`P/BV` > 0 & x$`P/E` > 0 & x$`P/S` < 2 &
                   x$`P/S` > 0,]
  
  na.omit(sm.model1) # Display clean data
}
smartlab.screener(sm_data1) # Test
