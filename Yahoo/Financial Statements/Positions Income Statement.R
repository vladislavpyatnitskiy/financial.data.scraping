library("rvest") # Library

is.positions.yahoo <- function(x, c){ # Positions from Income Statement
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
  
  y <- tab %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  # Find value, clean and make it numeric 
  y <- as.numeric(gsub(",","", gsub("([a-zA-Z]),","\\1 ", y[grep(c,y)+1])))[1] 
  
  names(y) <- c # Give names
  
  y # Display
}
is.positions.yahoo("AAPL", "EBITDA") # Test
