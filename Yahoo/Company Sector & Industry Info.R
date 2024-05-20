library("rvest") # Library

c.full.info <- function(x){ # info about company sector & industry
  
  l <- NULL # Create list
  
  for (n in 1:length(x)){ s <- x[n] # For each security find industry
  
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s))

    Y <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, cbind(y[3], y[5])) } # Add to list
    
  colnames(l) <- c("Sector", "Industry") # column names sectors & industries
  rownames(l) <- x # Tickers
  
  l # Display
}
c.full.info(x = c("AAPL", "NRG", "PVH", "C")) # Test
