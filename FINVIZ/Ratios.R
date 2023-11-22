finviz.ratios <- function(x){ # Function to get ratios from FINVIZ
  
  s<-sprintf("https://finviz.com/quote.ashx?t=%s&p=d", x)
  
  s.page <- read_html(s) # Read HTML of page
  
  s.yahoo <- s.page %>% html_nodes('table') %>% .[[10]] -> tab1 # Assign Table 
  
  s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df.f1 <- NULL # Lists to contain values
  df.f2 <- NULL
  
  for (n in 0:(length(s.header) / 2)){ 
    
    df.f1 <- rbind(df.f1, s.header[(1 + n * 2)]) # Ratio names
    
    df.f2 <- rbind(df.f2, s.header[(2 + n * 2)]) } # Ratio values
  
  df.f3 <- data.frame(df.f1, df.f2) # Join 
  
  df.f3 <- df.f3[-nrow(df.f3),] # Reduce excessive rows
  
  df.f3 # Display 
}
finviz.ratios("C") # Test
