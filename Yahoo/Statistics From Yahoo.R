library("rvest") # Library

statistics.yahoo <- function(x, y = 1, transpose = F){ # Statistics for Stocks
  
  DF <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ j <- x[n] # For each security get info
  
    s <- sprintf("https://uk.finance.yahoo.com/quote/%s/key-statistics?p=%s",
                 j, j)
    
    s.page <- read_html(s) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[y]] -> tab # Assign Table 
    
    s <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    D <- NULL # Data Frame values, like names for ratios and numbers
    
    for (n in 0:(length(s)/2)){ D <- rbind(D, cbind(s[(1+n*2)], s[(2+n*2)])) } 
    
    D <- D[-nrow(D),] # Reduce excessive row
    
    rownames(D) <- D[,1] # Assign row names
    
    D <- subset(D, select = -c(1)) # Reduce excess column
    
    colnames(D) <- j # Assign column name
    
    if (is.null(DF)){ DF <- D } else { DF <- cbind(DF, D) } }
    
  if (isTRUE(transpose)){ t(DF) } else { DF } # Display
}
statistics.yahoo(x = c("AAPL", "MSFT"), 1, transpose = F) # Test
