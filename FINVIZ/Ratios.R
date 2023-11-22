finviz.ratios <- function(x){ df.f4 <- NULL
  
  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
  
    s <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j))
    
    s.yahoo <- s %>% html_nodes('table') %>% .[[10]] -> tab1 # Assign Table 
    
    s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df.f1 <- NULL # Create lists to contain values
    df.f2 <- NULL
    
    for (n in 0:(length(s.header) / 2)){ 
      
      df.f1 <- rbind(df.f1, s.header[(1 + n * 2)]) # Ratio names
      
      df.f2 <- rbind(df.f2, s.header[(2 + n * 2)]) } # Ratio values
    
    df.f3 <- data.frame(df.f1, df.f2) # Join 
    
    df.f3 <- df.f3[-nrow(df.f3),] # Reduce last column
    
    df.f3[27,1] <- "EPS next Y (%)" # Change name
    
    rownames(df.f3) <- df.f3[,1] # Assign row names
    
    df.f3 <- subset(df.f3, select = -c(1)) # Reduce excess column
    
    colnames(df.f3) <- j # Assign column name
    
    if (is.null(df.f4)){ df.f4<-df.f3 } else { df.f4 <- cbind(df.f4,df.f3) } }
  
  df.f4 # Display
}
finviz.ratios(x = c("AAPL", "AMZN", "GOOGL", "META", "MSFT")) # Test
