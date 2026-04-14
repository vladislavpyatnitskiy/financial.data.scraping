lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

# Correlation coefficients for Russian stocks
moex.correlation <- function(x, method="spearman"){
  
  p <- NULL
  l <- NULL
  
  redom = list(
    c("AGRO", "RAGR"), c("CIAN", "CNRU"), c("HHRU", "HEAD"), c("FIVE", "X5"),
    c("FIXP", "FIXR"), c("YNDX", "YDEX"))
  
  from = "2007-01-01"
  
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
    
    l <- rbind.data.frame(l, cbind(rownames(R)[1], rownames(R)[nrow(R)]))
    p <- cbind(p, R) 
    R <- NULL  # Reset R for next iteration
  }
  
  colnames(p) <- x # Column names
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  L <- list(
    cor(as.matrix(diff(log(as.timeSeries(p)))[-1,]), method=method),
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
  
  colnames(L[[3]]) <- c("Start Date", "End Date")
  
  L
}
moex.correlation(c("LKOH", "SBER", "MGNT", "BELU"))
