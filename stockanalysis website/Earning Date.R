library("rvest") # Library

sa.earning <- function(x){ # Nearest Earning dates for stocks
  
  l <- NULL
  
  for (n in 1:length(x)){ y <- x[n]
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                   tolower(y))) %>% html_nodes('table') %>% .[[2]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    l <- rbind.data.frame(l, p[grep("Earnings Date", p) + 1]) }
  
  rownames(l) <- x # Tickers
  colnames(l) <- "Earnings Date" # Earning Date column name
  
  l # Display
}
sa.earning(c("AIG", "UNM")) # Test
