library("rvest") # library

rtsi.list.wiki <- function(x){ # Tickers from RTS Index
  
  s <- read_html(paste("https://ru.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[5]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # Get HTML for index data with tickers
  
  gsub('["\n"]', '', s[seq(from = 2, to = length(s), by = 7)]) # Display
}
rtsi.list.wiki("Индекс_РТС") # Test
