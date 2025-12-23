lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

beta.moex <- function(y){ # Beta values for Russian stocks
  
  y <- c(y, "IMOEX") # Add benchmark to list
    
  p <- NULL # Empty variables for security values & Data upload
  
  start_date <- as.Date(Sys.Date() - 365 * 5) # Start Date
  
  for (A in y){ D <- as.data.frame(get_candles(A, start_date,
                                               till = as.Date(Sys.Date()),
                                               interval = 'daily')[,c(3,8)])

    message(
      sprintf(
        "%s is downloaded; %s from %s", 
        A, which(y == A), length(y)
      )
    )
  
    D <- D[!duplicated(D),] # Remove duplicates
    
    p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- y # Put the tickers in data set
    
  p <- diff(log(as.timeSeries(p)))[-1,] # Time Series Returns and NA off
    
  B <- apply(p[, -which(names(p) == "IMOEX")], 2,
             function(col) ((lm((col) ~ p[,"IMOEX"]))$coefficients[2]))
  
  B <- as.data.frame(round(B, 2)) # Round by 2 decimal numbers
  
  colnames(B) <- "Beta" # Column name
  
  return(B) # Display values
}
beta.moex(c("LKOH", "RASP")) # Test
