# Function to Scrape Statistics for Security
statistics.yahoo <- function(x){ # Security URL
  
  s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s", x, x)
  
  s.page <- read_html(s) # Read HTML of page
  
  s.yahoo <- s.page %>% html_nodes('table') %>% .[[1]] -> tab1 # Assign Table 
  
  s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df.f1 <- NULL # Create lists for ratio names and values
  df.f2 <- NULL
  
  for (n in 0:(length(s.header) / 2)){ 
    
    df.f1 <- rbind(df.f1, s.header[(1 + n * 2)]) # Ratio names
    
    df.f2 <- rbind(df.f2, s.header[(2 + n * 2)]) } # Ratio values
  
  df.f3 <- data.frame(df.f1, df.f2) # Join 
  
  df.f3 <- df.f3[-nrow(df.f3),] # Display
  
  rownames(df.f3) <- df.f3[,1] # Assign row names
  
  df.f3 <- subset(df.f3, select = -c(1)) # Reduce excess column
  
  colnames(df.f3) <- x # Assign column name
  
  df.f3 # Display value
}
statistics.yahoo("AAPL") # Test
