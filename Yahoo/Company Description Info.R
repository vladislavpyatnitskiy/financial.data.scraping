library("rvest") # Library

c.description <- function(x){ l <- NULL # Get Company Info

  for (n in 1:length(x)){ s <- x[n]
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", s, s)
      
    page.p <- read_html(p) # Read HTML & extract necessary info
      
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
      
    y <- tab %>% html_nodes('p') %>% html_text() # Transform to text
      
    l <- list(l, y[length(y) - 1]) } # Add description to list
    
  l # Display
}
c.description(c("AAPL", "VALE", "X")) # Test
