library("rvest") # library

smartlab.info <- function(x){ # Current Stock Price Data
  
  p <- read_html(x) # Get info from website
  
  div <- p %>% html_nodes('table') %>% .[[1]] -> tab # Table
  
  f <- tab %>% html_nodes('tr') #%>% html_nodes('td') %>% html_text()
  
  L <- NULL # Reorganise data into data frame
  
  for (n in 2:length(f)){ l <- f[n] %>% html_nodes('td') %>% html_text()
    
    L <- rbind.data.frame(L, cbind(l[3], l[7])) } # Info about ticker and price
  
  colnames(L) <- c("ticker", "price") # Column names
  
  L # Display
}
smartlab.info("https://smart-lab.ru/q/shares/") # Test
