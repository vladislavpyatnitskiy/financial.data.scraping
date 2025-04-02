library("rvest") # Library

sa.earning <- function(x, sort=T, desc=F){ # Nearest Earning dates for stocks
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n] # Get data for each ticker 
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                   tolower(y))) %>% html_nodes('table') %>% .[[2]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    l <- c(l, format(as.Date(p[grep("Earnings Date", p) + 1],
                             format = "%B %d, %Y"), "%Y/%m/%d")) }
  names(l) <- x # Tickers
  
  if (isTRUE(sort)){ # If needed sorted, choose between asc and desc
    
    if (isTRUE(desc)){ l <- sort(l, decreasing = T) } else { l <- sort(l) } }
  
  l <- as.data.frame(l) # Change Data Format to Data Frame
  
  colnames(l) <- "Earnings Date" # Earning Date column name
  
  l # Display
}
sa.earning(c("FL", "UNM", "PVH"), T, T) # Test
