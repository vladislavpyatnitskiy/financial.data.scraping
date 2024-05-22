library("rvest") # Library

ratios.yahoo <- function(x, y){ m <- NULL # List for values

  for (n in 1:length(x)){ s <- x[n] # For each stock find data
    
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/%s", s,
                           "key-statistics"))
    
    Y <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    k <- NULL # List for ratio values
    
    for (l in 1:length(y)){ k <- cbind(k, i[grep(y[l], i) + 1]) }
    
      m <- rbind(m, k) } # Join data for all stocks
    
  rownames(m) <- x # Tickers
  colnames(m) <- y # Ratios
  
  m # Display values
}
ratios.yahoo(c("AAPL", "AMZN", "GOOGL"), c("Market cap", "Price/book"))
