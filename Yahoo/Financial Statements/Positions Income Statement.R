is.positions.yahoo <- function(x, c){ # Positions from Income Statement
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  y <- tab11 %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  y <- y[grep(c, y) + 1] # Take next position 
  
  y <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", y)) # Reduce commas
  
  y <- as.numeric(y)[1] # Make it numeric
  
  names(y) <- c # Give names
  
  y # Display
}
is.positions.yahoo("AAPL", "EBITDA") # Test
