library("rvest") # Library

finviz.country <- function(x){ l <- NULL # Get Country Info for each stock

  for (n in 1:length(x)){ s <- x[n]
    
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", s)) %>%
      html_nodes('body') %>% .[[1]] %>% html_nodes('div') %>%
      html_elements('a') %>% html_text()   
    
    l <- rbind(l, y[6]) } # Join
    
  colnames(l) <- "Country" # Column name
  rownames(l) <- x # tickers
  
  l # Display
}
finviz.country(c("AAPL", "STLA")) # Test
