c.industry <- function(x){ # Function to get info about company type
  
  l <- NULL # Create list
  
  for (n in 1:length(x)){ s <- x[n] # For each security find industry
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", s, s)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  yahoo.header1 <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>%
    html_text()
  
  l <- rbind(l, yahoo.header1[4])} # Add to list
  
  colnames(l) <- "Sector" # 
  rownames(l) <- x #
  
  l # Display
}
c.industry(x = c("AAPL", "NRG", "PVH", "C")) # Test
