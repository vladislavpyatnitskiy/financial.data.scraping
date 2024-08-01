library("rvest")

dax.list.wiki <- function(x){ # List of tickers from German Index DAX
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[5]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # HTML to get data
  
  s[seq(from = 4, to = length(s), by = 7)] # Display tickers
}
dax.list.wiki("DAX") # Test
