library("rvest") # Library

stock.analysis.isin <- function(x){ # ISIN Number of the stock
  
  l <- NULL
  
  for (n in 1:length(x)){ i <- x[n]
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/company/", i))
    
    price.yahoo1 <- p %>% html_nodes('table') %>% .[[3]] -> tab
    
    B <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text() 
      
    l <- rbind(l, B[14]) } # Data Frame with ISIN Number
  
  rownames(l) <- x # Tickers
  colnames(l) <- B[13] # ISIN Number column name
  
  l # Display
}
stock.analysis.isin(c("AAPL", "AMZN")) # Test
