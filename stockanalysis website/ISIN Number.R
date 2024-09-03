library("rvest") # Library

stock.analysis.isin <- function(x){ # ISIN Number of the stock
  
  l <- NULL
  
  for (n in 1:length(x)){ i <- x[n]
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/company/",
                           i)) %>% html_nodes('table') %>% .[[3]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text() 
    
    l <- rbind(l, p[14]) } # Data Frame with ISIN Number
    
  rownames(l) <- x # Tickers
  colnames(l) <- "ISIN" # ISIN
  
  l # Display
}
stock.analysis.isin(c("AAPL", "AMZN")) # Test
