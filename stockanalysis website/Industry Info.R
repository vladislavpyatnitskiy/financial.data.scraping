library(rvest) # Library

sa.industry <- function(x){ # Sector data from stockanalytics website
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 
    
    l <- rbind.data.frame(l, p[grep("industry", p)] %>% html_text()) } 
    
  rownames(l) <- x # tickers
  colnames(l) <- "Industry" # Column name
  
  l # Display
}
sa.industry(c("STLA", "X")) # Test
