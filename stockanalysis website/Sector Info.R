library(rvest) # Library

sa.sector <- function(x){ # Sector data from stockanalytics website
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/", tolower(y)))
    
    p <- p %>% html_nodes('body') %>% html_nodes('main') %>%
      html_nodes('div') %>% html_nodes('a') 
    
    q <- p %>% html_attr('class') == "dothref text-default" # Clean data & Join
    
    l <- rbind.data.frame(l, p[grep("TRUE", q)] %>% html_text() %>% .[2]) } 
    
  rownames(l) <- x # tickers
  colnames(l) <- "Sector" # Column name
  
  l # Display
}
sa.sector(c("STLA", "X")) # Test
