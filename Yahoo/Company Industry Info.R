library("rvest") # Library

c.industry <- function(x){ l <- NULL # Create list

  for (n in 1:length(x)){ s <- x[n] # For each security find industry
    
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s))
    
    Y <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, y[grep("Industry", y) + 1]) } # Add to list
    
  colnames(l) <- "Industry" # 
  rownames(l) <- x #
  
  l # Display
}
c.industry(x = c("AAPL", "NRG", "PVH", "C")) # Test
