library("rvest") # Library

finviz.ratios <- function(x){ v <- NULL # Where to store values

  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
  
    s <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j)) %>%
      html_nodes('table') %>% .[[10]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text()
    
    d <- data.frame(s[seq(from=1,to=length(s),by=2)], s[seq(from=2,to=length(s),
                                                            by=2)])
    d[27,1] <- "EPS next Y (%)" # Change name
      
    rownames(d) <- d[,1] # Assign row names
      
    d <- subset(d, select = -c(1)) # Reduce excessive column
      
    colnames(d) <- j # Assign column name
      
    if (is.null(v)){ v <- d } else { v <- cbind(v, d) } } # Join
      
  v # Display
}
finviz.ratios(x = c("AAPL", "AMZN", "GOOGL", "META", "MSFT")) # Test
