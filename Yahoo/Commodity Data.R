library("rvest") # Library

commodities.yahoo <- function(){ # Data Frame with Commodity values
  
  p1 <- read_html("https://finance.yahoo.com/commodities/") %>% 
    html_nodes('table') %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_nodes('div') # Read HTML
  
  p <- p1 %>% html_nodes('span') %>% html_text() # Extract names 
  
  v <- as.numeric(gsub(",", "", p[seq(from = 3, to = length(p), by = 3)]))
  
  tickers <- gsub(" ", "", p[seq(from = 1, to = length(p), by = 3)]) # Tickers
  
  p1 <- p1[seq(from = 3, to = length(p1), by = 6)] %>% html_text() # Names

  df <- cbind.data.frame(p1, v) # merge names with values

  rownames(df) <- tickers 
  colnames(df) <- c("Commodity", "Points") # Column names

  df # Display
}
commodities.yahoo() # Test
