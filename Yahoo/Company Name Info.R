library("rvest") # Library

s.names <- function(x){ # Data Frame with tickers and names
  
  l <- NULL
  
  for (n in 1:length(x)){ a <- x[n]
  
    s<-sprintf("https://uk.finance.yahoo.com/quote/%s?p=%s&.tsrc=fin-srch",a,a)
    
    s <- read_html(s) # Read html info
    
    s.yahoo <- s %>% html_nodes('body') %>% .[[1]] -> tab # Assign Body
    
    y <- tab %>% html_nodes('div') %>% html_nodes('h1') %>% html_text()
    
    y <- read.fwf(textConnection(y), widths = c(nchar(y) - nchar(a) - 3, 1),
                  colClasses = "character") # Reduce excessive elements
    
    l <- rbind(l, y[,-ncol(y)]) } # Join names
    
  s.list <- data.frame(x, l) # Join tickers with names
  
  rownames(s.list) <- seq(nrow(s.list)) # row names
  colnames(s.list) <- c("Ticker", "Name") # Column names
  
  s.list # Display
}
s.names("C") # Test
