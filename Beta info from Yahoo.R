beta.yahoo <- function(x){ # Function to get info about company beta
  
  b.values <- NULL # Create list to contain values
  
  for (n in 1:length(x)){ v <- x[n] # For every ticker get beta value
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- data.frame(i[grep("Beta ", i) + 1]) # Scrape Beta value
    
    rownames(b) <- v # Assign row names
    
    colnames(b) <- "Beta 5Y" # Assign column names
    
    b.values <- rbind.data.frame(b.values, b)} # Join betas
  
  b.values # Display
}
beta.yahoo(x = c("M", "X", "C", "AAPL")) # Test
