library("rvest") # Library

commodities.yahoo <- function(x){ # Data Frame with Commodity values
  
  p <- read_html(x) %>% html_nodes('table') %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # HTML
  
  l <- data.frame(p[seq(from = 2, to = length(p), by = 9)],
                  gsub(",", "", p[seq(from = 3, to = length(p), by = 9)]))
  
  colnames(l) <- c("Commodity", "Price") # Column names
  
  l # Display
}
commodities.yahoo("https://finance.yahoo.com/commodities/") # Test
