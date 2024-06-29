library("rvest") # Library

s.names <- function(x){ # Data Frame with tickers and names
  
  l <- NULL # Store data
  
  for (n in 1:length(x)){ a <- x[n] # Get names for all securities in the list
    
    s <- read_html(sprintf("https://finance.yahoo.com/quote/%s/", a))
    
    tab <- s %>% html_nodes('body') %>% .[[1]] # Assign Body
    
    y <- tab %>% html_nodes('div') %>% html_nodes('h1') # 
    
    y <- y[2] %>% html_text() # Subtract company name and clean value
  
    l <- rbind(l, read.fwf(textConnection(y), widths=c(nchar(y)-nchar(a)-3, 1),
                           colClasses = "character")[,1]) } # Join names
    
  s.list <- data.frame(x, l) # Join tickers with names
  
  rownames(s.list) <- seq(nrow(s.list)) # row names
  colnames(s.list) <- c("Ticker", "Name") # Column names
  
  s.list # Display
}
s.names(c("C", "X", "AAPL")) # Test
