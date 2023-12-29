library("rvest") # Library

# Function to Scrape Statistics for Security
statistics.yahoo <- function(x, y = 1, transpose = F){ # Security URL
  
  df.s <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ j <- x[n] # For each security get info
  
    s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",j,j)
    
    s.page <- read_html(s) # Read HTML of page
    
    s.yahoo <- s.page %>% html_nodes('table') %>% .[[y]] -> tab # Assign Table 
    
    s <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df.f2 <- NULL # Data Frame values
    
    for (n in 0:(length(s) / 2)){ df.f1 <- NULL # Row Values
      
      for (m in seq(2)){ df.f1 <- cbind(df.f1, s[(m + n * 2)]) }
      
      df.f2 <- rbind(df.f2, df.f1) } # Ratio values
    
    df.f2 <- df.f2[-nrow(df.f2),] # Display
    
    rownames(df.f2) <- df.f2[,1] # Assign row names
    
    df.f2 <- subset(df.f2, select = -c(1)) # Reduce excess column
    
    colnames(df.f2) <- j # Assign column name
    
    if (is.null(df.s)){ df.s <- df.f2 } else { df.s <- cbind(df.s, df.f2) } }
    
  if (isTRUE(transpose)){ t(df.s) } else { df.s } # Display
}
# Test
statistics.yahoo(x = c("AAPL", "MSFT", "GOOGL", "AMZN", "META"),1, transpose=F)
