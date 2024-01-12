library("rvest") # Library

cf.positions.yahoo <- function(x, c){ # Positions from Cash Flow Statement
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/cash-flow?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab 
  
  y <- tab %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  # Find value, clean and make it numeric 
  y <- as.numeric(gsub(",","",gsub("([a-zA-Z]),","\\1 ",y[grep(c,y)+1])))[1] 
  
  names(y) <- c # Give names
  
  y # Display
}
cf.positions.yahoo("AAPL", "Free Cash Flow") # Test
