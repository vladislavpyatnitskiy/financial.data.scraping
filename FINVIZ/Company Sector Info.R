library("rvest") # Library

finviz.sector <- function(x){ l <- NULL # Where to contain values

  for (n in 1:length(x)){ s <- x[n] # Get sector info for each ticker
  
    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", s)) %>%
      html_nodes('body') %>% .[[1]] %>% html_nodes('div') %>%
      html_elements('a') %>% html_text()   
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        s, which(x == s), length(x)
      )
    )
    
    l <- rbind(l, y[4]) } # Join
    
  colnames(l) <- "Sector" # Column name for data frame
  rownames(l) <- x # tickers
  
  l # Display
}
finviz.sector(c("AAPL", "F")) # Test
