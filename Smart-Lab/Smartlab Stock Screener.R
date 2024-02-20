# Use Data Frame from function enabling to get data from Smartlab website
smartlab.screener <- function(x){ # Screener for stocks
  
  for (n in 2:ncol(x)){ x[,n] <- as.numeric(x[,n]) } # Make data numeric
  
  # Screener settigns
  sm.model1 <- x[x$`EV/EBITDA` < 10 & x$`Debt/EBITDA` < 2 & x$`P/E` < 5 &
                   x$`P/BV` < 1 & x$`EV/EBITDA` > 0 & x$`Debt/EBITDA` > 0 &
                   x$`P/BV` > 0 & x$`P/E` > 0 & x$`P/S` < 1 & x$`P/S` > 0,]
  
  na.omit(sm.model1) # Display clean data
}
smartlab.screener(sm_data1) # Test
