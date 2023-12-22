c.executives <- function(x){ # Get info about executives
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", x, x)
    
  page.p <- read_html(p) # Read HTML & extract necessary info
    
  price.yahoo1 <- page.p %>% html_nodes('table') %>% .[[1]] -> tab11
    
  yahoo.header1 <- tab11 %>% html_nodes('tr') %>% html_nodes('td') %>%
    html_text()
  
  df.f1 <- NULL
  df.f2 <- NULL
  df.f3 <- NULL
  df.f4 <- NULL
  df.f5 <- NULL
  
  for (n in 0:(length(yahoo.header1) / 5)){ # Create table
    
    df.f1 <- rbind(df.f1, yahoo.header1[(1 + n * 5)]) # Name
    
    df.f2 <- rbind(df.f2, yahoo.header1[(2 + n * 5)]) # Title
    
    df.f3 <- rbind(df.f3, yahoo.header1[(3 + n * 5)]) # Pay
    
    df.f4 <- rbind(df.f4, yahoo.header1[(4 + n * 5)]) # Exercised 
    
    df.f5 <- rbind(df.f5, yahoo.header1[(5 + n * 5)]) } # Year Born
  
  df.f6 <- data.frame(df.f1, df.f2, df.f3, df.f4, df.f5) # Join 
  
  df.f6 <- df.f6[-nrow(df.f6),] # Subset excess row
  
  colnames(df.f6) <- c("Name", "Title", "Pay", "Exercised", "Year Born")
  
  df.f6 # Output
}
c.executives("AAPL") # Test
