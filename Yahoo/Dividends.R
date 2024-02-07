lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

s.dividends <- function(x, s = NULL, e = NULL){ # Dividends data
  
  d <- NULL # Create an empty variable
  
  for (A in x){ if (is.null(s) && is.null(e)) { # When no start or end date
    
    if (is.null(d)){ d <- getDividends(A)
        
        r.names <- rownames(d)
        
        d <- cbind(r.names, d) } else { d2 <- getDividends(A)
        
        r.names2 <- rownames(d2)
        
        d2 <- cbind(r.names2, d2)
        
        d<-merge(d, d2, all=T) } # When there is start date:
    
    } else if (is.null(e)) { if (is.null(d)){ d<-getDividends(A,from=s)
        
    r.names <- rownames(d)
    
    d <- cbind(r.names, d) } else { d2 <- getDividends(A,from=s)
      
      r.names2 <- rownames(d2)
      
      d2 <- cbind(r.names2, d2)
      
      d<-merge(d, d2, all=T) } # When there is end date:
      
    } else if (is.null(s)) { if (is.null(d)){ d<-getDividends(A,to=e)
       
    r.names <- rownames(d)
    
    d <- cbind(r.names, d) } else { d2 <- getDividends(A,to=e)
          
      r.names2 <- rownames(d2)
      
      d2 <- cbind(r.names2, d2)
      
      d<-merge(d, d2, all=T) } # When there are both start and end dates:
    
    } else { if (is.null(d)){ d<-getDividends(A,from=s,to=e)
      
    r.names <- rownames(d)
    
    d <- cbind(r.names, d) } else { d2<-getDividends(A,from=s,to=e)
        
      r.names2 <- rownames(d2)
      
      d2 <- cbind(r.names2, d2)
      
      d<-merge(d, d2, all=T) } } } # End of data collection
  
  d[is.na(d)] <- 0 # Substitute NA with 0
  
  colnames(d) <- x # Assign column names
  
  as.timeSeries(d) # Make them Time Series
}
s.dividends(c("AAPL", "MSFT"), s = "2023-01-01") # Test
