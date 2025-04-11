lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

s.names <- function(x){ # Data Frame with tickers and names
  
  l <- NULL
  
  for (n in 1:length(x)){ s <- x[n]
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/", s,
                            "key-statistics"), add_headers(`User-Agent` = B))
    
    y <- read_html(response) %>% html_nodes('body') %>% .[[1]] %>%
      html_nodes('div') %>% html_nodes('h1') %>% .[2] %>% html_text() 
    
    l <- rbind(l, read.fwf(textConnection(y),
                           widths = c(nchar(y) - nchar(s) - 3, 1),
                           colClasses = "character")[,1]) } # Join names
    
  DF <- data.frame(x, l) # Join tickers with names
  
  rownames(DF) <- seq(nrow(DF)) # row names
  colnames(DF) <- c("Ticker", "Name") # Column names
  
  DF # Display
}
s.names("AAPL") # Test
