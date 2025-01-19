library(rvest) # Library

rating.sa <- function(x){ # Company Ratings
  
  D <- NULL
  
  for (n in 1:length(x)){ l <- c("Price Target", "Upside")
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/ratings/",
                           tolower(x[n]))) %>% html_nodes('main') %>%
      html_nodes('div') %>% html_nodes('div') %>% html_text()
    
    d <- NULL # Info with Predicted Price and Upside
    
    for (m in 1:2){ f <- unlist(strsplit(p[grepl(l[m], p)][1], " "))
    
      d <- c(d, f[length(f)]) } # Join into vector
    
    D <- rbind.data.frame(D, cbind(p[9], d[1], d[2])) } # Merge values
    
  colnames(D) <- c("Current Price", "Predicted Price", "Upside") # Column names
  rownames(D) <- x # Tickers
  
  D # Display
}
rating.sa(c("VIRT", "C")) # Test
