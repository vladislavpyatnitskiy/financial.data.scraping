library("rvest") # Library

finviz.sectors.dividends <- function(x){ # Dividends Yield for each sector
  
  s <- read_html(sprintf("https://finviz.com/%s", x)) %>%
    html_nodes('table') %>% .[[8]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text() # Get data
  
  d <- data.frame(s[seq(from=2,to=length(s),by=11)], s[seq(from=5,to=length(s),
                                                          by=11)])
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Dividend Yield (%)") # Assign Column name
  
  for (n in 1:nrow(d)){ # Reduce "%" from Dividend Yield column values
    
    d[n,1] <- read.fwf(textConnection(d[n,1]), widths = c(nchar(d[n,1]) - 1, 1),
                       colClasses = "character")[,1] }
  
  for (n in 1:ncol(d)){ d[,n] <- as.numeric(d[,n]) } # Make data numeric
  
  d # Display
}
finviz.sectors.dividends("groups.ashx?g=sector&v=110&o=name") # Test
