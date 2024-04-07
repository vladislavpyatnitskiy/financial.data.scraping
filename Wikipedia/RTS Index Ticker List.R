library("rvest") # library

rtsi.list.wiki <- function(x){ # Tickers from RTS Index
  
  s <- read_html(paste("https://ru.wikipedia.org/wiki/", x, sep = ""))
  
  s.wiki <- s %>% html_nodes('table') %>% .[[5]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Store tickers
  
  for (n in 0:(length(y) / 7)){ # Clean data
    
    v <- rbind.data.frame(v, gsub('["\n"]', '', y[2 + n * 7])) }
  
  v <- v[apply(v, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  v # Display
}
rtsi.list.wiki("Индекс_РТС") # Test
