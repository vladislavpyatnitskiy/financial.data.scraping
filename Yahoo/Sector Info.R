library("rvest") # Library

c.sector <- function(x){ l <- NULL # Create list

  for (n in 1:length(x)){ s <- x[n] # For each security find sector
  
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s))
    
    Y <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') %>% html_nodes('span') %>% html_text()
    
    l <- rbind(l, y[grep("Sector", y) + 1]) } # Add to list
    
  colnames(l) <- "Sector" # 
  rownames(l) <- x #
  
  l # Display
}
c.sector(x = c("AAPL", "NRG", "PVH", "C")) # Test
