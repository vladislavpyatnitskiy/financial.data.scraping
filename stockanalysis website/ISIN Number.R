library("rvest") # Library

stock.analysis.isin <- function(x){ # ISIN Number of the stock
  
  l <- NULL
  
  for (n in 1:length(x)){ i <- x[n]
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/company/",
                           i)) %>% html_nodes('table') %>% .[[3]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        i, which(x == i), length(x)
      )
    )
    
    l <- rbind(l, p[grep("ISIN Number", p) + 1]) } # Data Frame of ISIN 
    
  rownames(l) <- x # Tickers
  colnames(l) <- "ISIN" # ISIN
  
  l # Display
}
stock.analysis.isin(c("AAPL", "AMZN")) # Test
