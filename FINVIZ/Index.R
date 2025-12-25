library("rvest") # Library

finviz.ratios.index <- function(x){ v <- NULL # Where to store values

  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page

    y <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j)) %>%
      html_nodes('div') %>% html_nodes('table') %>% html_nodes('tr') %>% 
      html_nodes('td') %>% html_nodes('small') %>% html_text() %>% .[1]   
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        j, which(x == j), length(x)
      )
    )
    
    v <- rbind(v, y) } # Join
    
  rownames(v) <- x
  colnames(v) <- "Index"
  
  v # Display
}
finviz.ratios.index(x = c("AAPL", "MSFT", "META", "AMZN", "GOOGL")) # Test
