library("rvest") # library

smartlab.info <- function(x){ # Current Stock Price Data
  
  f <- read_html(x) %>% html_nodes('table') %>% .[[1]] %>% html_nodes('tr') 
  
  L <- NULL # Reorganise data into data frame
  
  for (n in 2:length(f)){ l <- f[n] %>% html_nodes('td') %>% html_text()
  
    L <- rbind.data.frame(L, cbind(l[3], l[7])) } # Info about ticker and price
  
  colnames(L) <- c("ticker", "price") # Column names
  
  L # Display
}
smartlab.info("https://smart-lab.ru/q/shares/") # Test
