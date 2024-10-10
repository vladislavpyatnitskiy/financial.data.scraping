lapply(c("quantmod", "timeSeries"), require, character.only = T) # Libs

s.dividends <- function(x, s = NULL, e = NULL) { # Get Dividends data
  
  d <- NULL # For stocks paying dividends
  no.d <- NULL # For stocks not paying dividends
  
  getDivData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getDividends(A)) # No period defined
    if (is.null(e)) return(getDividends(A, from = s)) # Start Date is defined
    if (is.null(s)) return(getDividends(A, to = e)) # End Date is defined
    return(getDividends(A, from = s, to = e)) # Start and End Date
  }
  
  for (A in x) {
    d2 <- getDivData(A, s, e) # Get data
    
    if (isTRUE(length(d2) == 0)) { # if stock no paying dividend, add to list
      no.d <- c(no.d, A)
    } else {
      r.names2 <- rownames(d2) # otherwise, prepare data frame to merge
      d2 <- cbind(r.names2, d2)
      
      if (is.null(d)) {
        d <- d2
      } else {
        d <- merge(d, d2, all = T) # Merge by dates
      }
    }
  }
  
  if (!is.null(d)) { # if there are dividend paying companies, 
    d[is.na(d)] <- 0 # Assign Not available values as 0
    d <- as.timeSeries(d)
    k <- colnames(d)
    
    if (length(no.d) > 0) { # to merge not paying companies with others
      r.names3 <- as.data.frame(rownames(d))
      r.names5 <- NULL
      
      for (n in no.d) {
        r.names4 <- data.frame(r.names3, 0)
        colnames(r.names4) <- c("Date", n)
        r.names5 <- if (is.null(r.names5)) r.names4 else merge(r.names5,
                                                               r.names4,
                                                               by = "Date")
      }
      rownames(r.names5) <- r.names5[, 1] # Put dates in row names 
      r.names5 <- r.names5[, -1] # Reduce date values from main data frame
      d <- cbind(d, as.timeSeries(r.names5)) # Make it in time series type
      k <- c(k, no.d) # Put ticker names by order it got while extracting 
    }
    colnames(d) <- gsub('[".div"]', '', k) # Reduce ".div" from ticker names
    d <- d[, x] # Put at the same order as typed in function
  }
  return(d) # display
}
s.dividends(c("AAPL","MSFT","AMZN","C","GOOGL","VSTO"), s="2022-07-19") # Test
