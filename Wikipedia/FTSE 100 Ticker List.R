library("rvest") # Library

ftse100.list.wiki <- function(x, yahoo = T){ # Tickers from FTSE 100
  
  s <- read_html(paste("https://en.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[5]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers
  
  for (n in 0:(length(y) / 3)){ # Clean data
    
    if (isTRUE(yahoo)){ # If you need data for Yahoo! Finance
    
      v <- rbind.data.frame(v, paste(y[2 + n * 3], ".L", sep = "")) } else {
        
        v <- rbind.data.frame(v, y[2 + n * 3]) } } # Otherwise
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
ftse100.list.wiki("FTSE_100_Index", yahoo = F) # Test
