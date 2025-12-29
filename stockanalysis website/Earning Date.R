library("rvest") # Library

sa.earning <- function(x, sort=F, desc=F, ts=F){ # Nearest Earning dates 
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n]
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('table') %>% .[[2]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        x[n], which(x == x[n]), length(x)
      )
    )
    
    l <- c(l, format(as.Date(p[grep("Earnings Date", p) + 1],
                             format = "%B %d, %Y"), "%Y-%m-%d")) }
    names(l) <- x # Tickers
    
    if (sort){ if (desc){ l <- sort(l, decreasing=T) } else { l <- sort(l) } }
    
    l <- as.data.frame(l)
    
    colnames(l) <- "Earnings Date" # Earning Date column name
    
    if (ts){ U <- unique(l[,1]) # Find unique values for dates
      
      tickers <- rownames(l) # Assign a variable to contain values for tickers
      
      d <- l[,1] # Reduce data frame to vector
      
      names(d) <- tickers # Assign names to vector
      
      m <- NULL # Group Stocks by Earning Dates
      
      for (n in 1:length(U)){ # Assign tickers to each date:
        
        if (!identical(names(which(d == U[n])), character(0))){
          
          m = rbind.data.frame(m, cbind(U[n],
                                        toString(names(which(d == U[n]))))) } }
      
    rownames(m) <- m[,1] # Assign dates as row names
    
    m <- subset(m, select = -c(1)) # Reduce excess column
    
    colnames(m) <- "Companies" # Column name
    
    l <- m  }
    
  l # Display
}
sa.earning(c("FL", "UNM", "PVH"), T, T, F) # Test
