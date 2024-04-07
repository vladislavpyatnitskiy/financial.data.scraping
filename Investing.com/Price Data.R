library("rvest") # Library

investing.com.data <- function(x){ 
  
  p <- read_html(sprintf("https://uk.investing.com/equities/%s-historical-data",
                         x))
  
  price.yahoo1 <- p %>% html_nodes('table') %>% .[[2]] -> tab
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  l <- NULL
  
  for (n in 1:(length(y) / 7)){
    
    l <- rbind.data.frame(l, cbind(y[1 + n * 7], y[2 + n * 7])) }
  
  ts <- l[,1] # Dates
  
  for (n in 1:length(ts)){ # Reform data format
    
    Y <- as.character(read.fwf(textConnection(ts[n]),
                               widths=c(nchar(ts[n]) - 4, nchar(ts[n]) - 0),
                               colClasses = "character")[2]) # Year
    
    M <- as.character(read.fwf(textConnection(ts[n]),
                               widths=c(nchar(ts[n]) - 7, nchar(ts[n]) - 8),
                               colClasses = "character")[2]) # Month
    
    d <- as.character(read.fwf(textConnection(ts[n]),
                               widths=c(nchar(ts[n]) - 8, nchar(ts[n]) - 10),
                               colClasses = "character")[1]) # Day
    
    ts[n] <- paste(Y, M, d, sep = "-") } # Concatenate dates
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column
  
  l <- data.frame(ts, l)
  
  l <- l[order(l[,1], decreasing = F),] # Order from the first to last date
  
  ts <- l[,1]
  
  l <- as.data.frame(l[,2])
  
  rownames(l) <- ts
  
  l
}
investing.com.data(x = "mmk_rts") # Test
