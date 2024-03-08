library("rvest") # Library

cac40.list.wiki <- function(x){ # Tickers from CAC 40
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[5]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers
  
  for (n in 0:(length(y) / 4)){ # Clean data
    
    v <- rbind.data.frame(v, gsub('["\n"]', '', y[4 + n * 4])) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
cac40.list.wiki("CAC_40") # Test
