library("rvest") # Library

finviz.ratios <- function(x){ # Get info about financial ratios for stocks
  
  v <- NULL # Where to store values
  
  for (n in 1:length(x)){ j <- x[n] # Assign security and read HTML of page
        
    s <- read_html(sprintf("https://finviz.com/quote.ashx?t=%s&p=d", j)) %>%
      html_nodes('table')
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        j, which(x == j), length(x)
      )
    ) # Download message
    
    y = s %>% html_attr('class') == 
      "js-snapshot-table snapshot-table2 screener_snapshot-table-body"
    
    l <- s[which(y)] %>% html_nodes('td') %>% html_text()
    
    d <- data.frame(
      l[seq(from = 1, to = length(l), by = 2)], 
      l[seq(from = 2, to = length(l), by = 2)]
      ) # Form data frame
    
    d[27,1] <- "EPS next Y (%)" # Change name
    
    rownames(d) <- d[,1] # Assign row names
    
    d <- subset(d, select = -c(1)) # Reduce excessive column
    
    colnames(d) <- j # Assign column name
    
    if (is.null(v)){ v <- d } else { v <- cbind(v, d) } } # Join
    
  v # Display
}
finviz.ratios(x = c("AAPL", "AMZN", "GOOGL", "META", "MSFT")) # Test
