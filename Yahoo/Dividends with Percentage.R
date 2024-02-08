lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

s.dividends.percentage <- function(x, s = NULL, e = NULL){
  
  d <- NULL # Create an empty variable
  
  for (A in x){ if (is.null(s) && is.null(e)) { # When no start or end date
    
    if (is.null(d)){ d <- getDividends(A)
    
        r.names <- rownames(d)
        
        d <- cbind(r.names, d) } else { d2 <- getDividends(A)
        
        r.names2 <- rownames(d2)
        
        d2 <- cbind(r.names2, d2)
        
        d<-merge(d, d2, all=T) } # When there is start date:
        
        } else if (is.null(e)) { if (is.null(d)){ d<-getDividends(A,from=s)
  
        if (isTRUE(length(d) == 0)){ d <- timeSeries(0) 
        
        rownames(d) <- s }
        
        r.names <- rownames(d)
        
        d <- cbind(r.names, d) } else { d2 <- getDividends(A,from=s)
        
        if (isTRUE(length(d2) == 0)){ d2 <- timeSeries(0.00) 
        
        rownames(d2) <- s }
        
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
  
  p <- NULL # Create an empty variable
  
  # Loop for data extraction & # Set up statements for start and end dates
  for (A in x){ if (is.null(s) && is.null(e)) {
    
    # When neither start date nor end date are defined
      p <- cbind(p, getSymbols(A, src = "yahoo", auto.assign=F)[,4])
    
    } else if (is.null(e)) { # When only start date is defined
    
      p <- cbind(p, getSymbols(A, from = s,src="yahoo",auto.assign=F)[,4])
    
    } else if (is.null(s)) { # When only end date is defined
    
      p <- cbind(p,getSymbols(A,to=e,src="yahoo",auto.assign=F)[,4])
    
    } else { # When both start date and end date are defined
    
      p<-cbind(p,getSymbols(A,from=s,to=e,src="yahoo",auto.assign=F)[,4]) } }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  d <- as.timeSeries(d) 
  
  rownames.p <- rownames(p) # Subtract dates column
  rownames.d <- rownames(d)
  
  d <- cbind(d, d) # Double dividend column
  
  p <- data.frame(rownames.p, p) # Put dates as a first column
  d <- data.frame(rownames.d, d)
  
  colnames(p)[1] <- "Date" # Change column name
  colnames(d)[1] <- "Date"
  
  rownames(d) <- seq(nrow(d)) # Substitute dates with ascending numbers
  rownames(p) <- seq(nrow(p))
  
  df <- merge(d, p, by = "Date") # Join dividends and prices
  
  df[,3] <- df[,3] / df[,4] # Divide dividends on prices
  
  rownames(df) <- df[,1] # Put dates into row names
  
  df <- df[,-1] # Reduce dates column
  df <- df[,-3] # Reduce prices column
  df[,2] <- round(df[,2] * 100, 2) # Round
  
  colnames(df) <- c("Dividends", "%") # Column names
  
  df # Display
}
s.dividends.percentage("VIRT", s = "2022-07-19") # Test
