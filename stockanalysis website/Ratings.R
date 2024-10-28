library(rvest) # Library

rating.sa <- function(x){ # Company Ratings
  
  D <- NULL
  
  for (n in 1:length(x)){ 
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/ratings/",
                           tolower(x[n])))
  
    p <- p %>% html_nodes('main') %>% html_nodes('div') %>%
      html_nodes('div') %>% html_text()
    
    D <- rbind.data.frame(D, cbind(p[9], p[27], p[30])) } # Merge values
    
  colnames(D) <- c("Current Price", "Predicted Price", "Upside") # Column names
  rownames(D) <- x # Tickers
  
  D # Display
}
rating.sa(c("AMZN", "C", "VALE")) # Test
