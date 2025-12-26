library("rvest") # Library

stock.analysis.country <- function(x){ # Countries for stock
  
  l <- NULL # Store countries for each security
  
  for (n in 1:length(x)){ i <- x[n] # Get Country for each security
    
    C = read_html(sprintf("https://stockanalysis.com/stocks/%s/company/",
                          i)) %>% html_nodes('table') %>% .[[1]] %>% 
      html_nodes('tr') %>% html_nodes('td') %>% html_text() 
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        i, which(x == i), length(x)
      )
    )
    
    l <- rbind.data.frame(l, C[grep("Country", C) + 1]) } # Data Frame 
    
  rownames(l) <- x # Tickers
  colnames(l) <- "Country" # Country column name
  
  l # Display
}
stock.analysis.country(c("AAPL", "ZIM", "STLA", "VALE")) # Test
