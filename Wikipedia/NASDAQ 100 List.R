library("rvest") # Library

nasdaq.list.wiki <- function(x){ # Get data with tickers
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[5]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL
  
  for (n in 0:(length(y) / 4)){ v <- rbind.data.frame(v, y[2 + n * 4]) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v <- as.vector(v) # Change format to vector
  
  v # Display
}
nasdaq.list.wiki("Nasdaq-100")
