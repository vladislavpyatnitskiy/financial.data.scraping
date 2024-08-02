library("rvest") # Library

sp500.list.wiki <- function(x){ # Tickers from S&P 500
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text()
  
  gsub('["\n"]', '', s[seq(from = 1, to = length(s), by = 8)]) # Display
}
sp500.list.wiki("List_of_S%26P_500_companies") # Test
