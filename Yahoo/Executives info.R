library("rvest")

c.executives <- function(x){ # Get info about executives
  
  p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", x))
  
  tab <- p %>% html_nodes('table') %>% .[[1]]
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  D <- NULL # Create space for data frame
  
  for (n in 1:(length(y) / 10)){ d <- NULL # Structure Data
  
    for (m in seq(0, 9)){ d <- rbind(d, y[(n + m * 5)]) }
    
    if (is.null(D)){ D <- d } else { D <- cbind(D, d) } } 
  
  colnames(D) <- c("Name", "Title", "Pay", "Exercised", "Year Born")
  
  D # Output
}
c.executives("AAPL") # Test
