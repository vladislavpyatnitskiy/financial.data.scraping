library("rvest") # Library

sp500.list.wiki <- function(x){ # Tickers from S&P 500
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[1]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers
  
  for (n in 0:(length(y) / 8)){ # Clean data
    
    v <- rbind.data.frame(v, gsub('["\n"]', '', y[1 + n * 8])) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
sp500.list.wiki("List_of_S%26P_500_companies") # Test
