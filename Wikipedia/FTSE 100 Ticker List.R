library("rvest") # Library

ftse100.list.wiki <- function(x, yahoo = T){ # Tickers from FTSE 100
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = "")) %>%
    html_nodes('table') %>% .[[5]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # HTML to get data
  
  if (isTRUE(yahoo)){ paste(s[seq(from=2, to=length(s), by=3)], ".L", sep="")
    } else { s[seq(from = 2, to = length(s), by = 3)] } # Display
}
ftse100.list.wiki("FTSE_100_Index", yahoo = T) # Test
