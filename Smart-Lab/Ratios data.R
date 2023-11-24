smartlab.ratios <- function(x, y){ # Function to get info from Smartlab
  
  s <- read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         x)) # Smartlab HTML
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[1]] -> tab000
  
  s.header <- tab000 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df.f1 <- NULL # Assign lists
  df.f2 <- NULL
  df.f3 <- NULL
  df.f6 <- NULL
  
  for (n in 0:(length(s.header) / 6)){ 
    
    df.f1 <- rbind(df.f1, s.header[(1 + n * 6)]) # Indices
    
    df.f2 <- rbind(df.f2, s.header[(2 + n * 6)]) # Names
    
    df.f3 <- rbind(df.f3, s.header[(3 + n * 6)]) # Tickers and then ratios
    
    df.f6 <- rbind(df.f6, as.numeric(gsub("\\D", "", s.header[(6 + n * 6)]))) }
  
  df.final <- data.frame(df.f1, df.f2, df.f3, df.f6) # Join
  
  df.final <- subset(df.final, select = -c(1)) # Reduce first column
  
  df.final <- df.final[-nrow(df.final),] # Reduce last row
  
  colnames(df.final) <- c("Название", "Тикер", y) # Column names
  
  df.final # Display
}
View(smartlab.ratios("ev_ebitda", "EV/EBITDA")) # Test
