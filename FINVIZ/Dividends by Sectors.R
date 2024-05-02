library("rvest") # Library

finviz.sectors.dividends <- function(x){ # Dividends Yield for each sector
  
  s <- read_html(sprintf("https://finviz.com/%s", x))
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[8]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  d <- NULL # Data Frame with values
  
  for (n in 0:(length(y) / 11)){ d<-rbind(d,cbind(y[(2+n*11)],y[(5+n*11)])) }
  
  d <- d[-nrow(d),] # Reduce last column
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Dividend Yield (%)") 
  
  for (n in 1:nrow(d)){ # Reduce "%" from Dividend Yield column values
    
    d[n,1] <- read.fwf(textConnection(d[n,1]), widths = c(nchar(d[n,1]) - 1, 1),
                       colClasses = "character")[,1] }
  
  d <- as.data.frame(d) # Transform to data frame
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  d # Display
}
finviz.sectors.dividends("groups.ashx?g=sector&v=110&o=name") # Test
