library("rvest") # Library

dowjones.list.wiki <- function(x){ # Show list of tickers from DJIA
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers
  
  for (n in 0:(length(y) / 6)){ # Clean data
    
    v <- rbind.data.frame(v, gsub('["\n"]', '', y[2 + n * 6])) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
dowjones.list.wiki("Dow_Jones_Industrial_Average") # Test
