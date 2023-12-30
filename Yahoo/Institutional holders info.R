library("rvest") # Library

s.holders <- function(x){ # info about holders of large positions
  
  s<-read_html(sprintf("https://finance.yahoo.com/quote/%s/holders?p=%s",x,x))
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[2]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  v <- NULL # Create lists to contain variables & Subset excess row
  
  for (n in 0:(length(y) / 5)){ # Scrape data for holders and portions
    
    v <- rbind(v,cbind(y[(1+n*5)],read.fwf(textConnection(y[(4 + n * 5)]),
                                           widths=c(nchar(y[(4 + n * 5)])-1,1),
                                           colClasses = "character"))) } 
  v <- v[-nrow(v),][,-ncol(v)] 
  
  v[,2] <- as.numeric(v[,2]) # Change format to numeric
  
  v[nrow(v) + 1,] = c("Others", 100 - sum(v[,2])) # Numbers for others
  
  colnames(v) <- c("Top Institutional Holders", "Portion (%)") # Column names
  rownames(v) <- seq(nrow(v)) # Row names
  
  v # Display
}
s.holders("AAPL") # Test
