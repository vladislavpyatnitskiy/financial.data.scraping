lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

beta.moex <- function(x){ # Beta values for Russian stocks
  
  x <- c(x, "IMOEX") # Add benchmark to list
  
  redom = list(
    c("AGRO", "RAGR"), c("CIAN", "CNRU"), c("HHRU", "HEAD"), c("FIVE", "X5"),
    c("FIXP", "FIXR"), c("YNDX", "YDEX"))
  
  from = "2007-01-01"
  
  J <- NULL
  R <- NULL
  
  for (n in 1:length(x)){
    
    if (any(sapply(redom, function(redom_item) x[n] %in% redom_item))){
      
      f <- which(sapply(redom, function(redom_item) x[n] %in% redom_item))
      
      for (k in 1:length(redom[[f]])){
        
        a = as.data.frame(
          get_candles(redom[[f]][k], from=from, interval='daily')[,c(3,8)]
        )
        
        if (k == 2){ 
          
          message(
            sprintf(
              "%s is downloaded; %s from %s", x[n], which(x == x[n]), length(x)
            )
          )
        }
        
        a <- a[!duplicated(a),] # Remove duplicates
        
        a <- xts(a[, 1], order.by = as.Date(a[, 2]))
        
        if (x[n] == "AGRO") a <- a / 7.01
        
        colnames(a) <- redom[[f]][2]
        
        if (is.null(R)) R <- data.frame(a) else R <- rbind.data.frame(R, a)
      }
    } else {
      
      a = as.data.frame(get_candles(x[n], from=from, interval='daily')[,c(3,8)])
      
      message(
        sprintf(
          "%s is downloaded; %s from %s", 
          x[n], which(x == x[n]), length(x)
        )
      )
      
      a <- a[!duplicated(a),] # Remove duplicates
      
      a <- xts(a[, 1], order.by = as.Date(a[, 2]))
      
      colnames(a) <- x[n]
      
      R <- data.frame(a) 
    }
    
    R <- as.timeSeries(R) # Make it time series
    
    if (x[n] == "BELU"){ j <- which(rownames(R) == "2024-08-15")
    
      R[c(1:j),] <- R[c(1:j),]/8 } # Adjustments for Novabev stock
    
    if (is.null(J)) J <- list(R) else J[[n]] <- R 
    R <- NULL  # Reset R for next iteration
  }
  
  B <- NULL
  
  p2 <- as.timeSeries(J[[length(J)]])
  
  for (n in 1:(length(J) - 1)){

    p <- cbind(as.timeSeries(J[[n]]), p2)
    
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    p <- diff(log(as.timeSeries(p)))[-1,] # Time Series Returns and NA off

    B <- rbind.data.frame(
      B, round(lm((p[,1]) ~ p[,"IMOEX"])$coefficients[2], 2)
      ) # Round by 2 decimal numbers
  }

  rownames(B) <- x[-length(x)]
  colnames(B) <- "Beta" # Column name

  return(B) # Display values
}
beta.moex(c("LKOH", "RASP")) # Test
