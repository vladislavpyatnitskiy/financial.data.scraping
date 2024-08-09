library(rvest) # Library

crypto.yahoo <- function(x){ # Data Frame with Cryptocurrency values
  
  p <- read_html(x) %>% html_nodes('table') %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # HTML
  
  l <- data.frame(p[seq(from = 1, to = length(p), by = 12)],
                  gsub(",", "", p[seq(from = 3, to = length(p), by = 12)]))
  
  colnames(l) <- c("Cryptocurrency", "Price") # Column names
  
  l # Display
}
crypto.yahoo("https://finance.yahoo.com/crypto/") # Test
