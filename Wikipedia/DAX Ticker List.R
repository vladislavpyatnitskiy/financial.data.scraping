library("rvest")

dax.list.wiki <- function(x){ # List of tickers from German Index DAX
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[5]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers and clean data
  
  for (n in 0:(length(y) / 7)){ v <- rbind.data.frame(v, y[4 + n * 7]) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
dax.list.wiki("DAX") # Test
