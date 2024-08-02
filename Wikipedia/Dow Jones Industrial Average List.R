library("rvest") # Library

dowjones.list.wiki <- function(x){ # Show list of tickers from DJIA
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[2]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # Get HTML for index data with tickers
  
  gsub('["\n"]', '', s[seq(from = 2, to = length(s), by = 6)]) # Display
}
dowjones.list.wiki("Dow_Jones_Industrial_Average") # Test
