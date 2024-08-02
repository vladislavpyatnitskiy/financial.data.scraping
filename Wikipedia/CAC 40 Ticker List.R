library("rvest") # Library

cac40.list.wiki <- function(x){ # Tickers from CAC 40
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[5]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # Get HTML for index data with tickers
  
  gsub('["\n"]', '', s[seq(from = 4, to = length(s), by = 4)]) # Display
}
cac40.list.wiki("CAC_40") # Test
