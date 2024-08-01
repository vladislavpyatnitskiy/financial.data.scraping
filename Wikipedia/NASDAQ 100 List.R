library("rvest") # Library

nasdaq.list.wiki <- function(x){ # Get data with tickers
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[5]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # HTML to get data
  
  s[seq(from = 2, to = length(s), by = 4)] # Display
}
nasdaq.list.wiki("Nasdaq-100") # Test
