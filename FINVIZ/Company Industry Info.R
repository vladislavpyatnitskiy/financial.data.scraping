library("rvest") # Library

finviz.industry <- function(x){ l <- NULL # Where to contain values

  for (n in 1:length(x)){ s <- x[n] # Get industry info for each ticker
    
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", s)) %>%
      html_nodes('body') %>% .[[1]] %>% html_nodes('div') %>%
      html_elements('a') %>% html_text()   
    
    l <- rbind(l, y[5]) } # Join
    
  colnames(l) <- "Industry" # Column name for data frame
  rownames(l) <- x # tickers
  
  l # Display
}
finviz.industry(c("AAPL", "F")) # test
