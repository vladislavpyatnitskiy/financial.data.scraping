library("rvest")

c.executives <- function(x){ # Get info about executives
  
  p<-read_html(sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s",x,x))
  
  p <- p %>% html_nodes('table') %>% .[[1]] -> tab
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df.f2 <- NULL # Create space for data frame
  
  for (n in 1:(length(y) / 10)){ c.df <- NULL # Structure Data
    
    for (m in seq(0, 9)){ c.df <- rbind(c.df, y[(n + m * 5)]) }
    
    if (is.null(df.f2)){ df.f2 <- c.df } else { df.f2 <- cbind(df.f2, c.df) } } 
  
  colnames(df.f2) <- c("Name", "Title", "Pay", "Exercised", "Year Born")
  
  df.f2 # Output
}
c.executives("AAPL") # Test
