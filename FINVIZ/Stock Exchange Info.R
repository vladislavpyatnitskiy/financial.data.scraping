library("rvest") # Library

finviz.exchange <- function(x){ l <- NULL # Info about company's stock exchange

  for (n in 1:length(x)){ s <- x[n] # Get info for each stock
    
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", s)) %>%
      html_nodes('body') %>% .[[1]] %>% html_nodes('div') %>%
      html_elements('a') %>% html_text() %>% .[7]   
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        s, which(x == s), length(x)
      )
    )
    
    l <- rbind(l, y) } # Join
    
  colnames(l) <- "Stock Exchange" # Column Name
  rownames(l) <- x # Tickers
  
  l # Display
}
finviz.exchange(c("AAPL", "STLA")) # Test
