library(rvest) # Library

crypto.yahoo <- function(){ # Data Frame with Crypto-currency values
  
  p1 <- read_html("https://finance.yahoo.com/crypto/") %>% 
    html_nodes('table') %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_nodes('div') # HTML
  
  p <- p1 %>% html_nodes('span') %>% html_text() # Extract names 
  
  v <- as.numeric(gsub(",", "", p[seq(from = 4, to = length(p), by = 6)]))
  
  tickers <- gsub(" ", "", p[seq(from = 3, to = length(p), by = 6)]) # Tickers
  
  v <- as.data.frame(v) # 
  
  rownames(v) <- tickers
  colnames(v) <- c("Price") # Column names
  
  v # Display
}
crypto.yahoo() # Test
