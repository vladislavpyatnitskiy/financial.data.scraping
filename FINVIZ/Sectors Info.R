library("rvest") # Library

finviz.sectors.table <- function(){ # Data Frame with info about sectors
  
  s <- read_html("https://finviz.com/groups.ashx?g=sector&v=120&o=name") %>%
    html_nodes('table') %>% .[[8]] %>% html_nodes('tr') %>% 
    html_nodes('td') %>% html_text() # Get data from web site
  
  l <- setdiff(2:10, 6)
  
  d <- NULL
  
  for (n in 1:length(l)){ # Market Cap & PE & FPE & PS & PB & PC & PFCF
    
    d <- rbind.data.frame(d, s[seq(from = l[n], to = length(s), by = 15)]) 
  }
  
  d <- as.data.frame(t(d)) # Transpose and change from matrix to data frame
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c(
    "Market Cap ($blns)", "P/E", "Forward P/E", "P/S", "P/B", "P/C", "P/FCF"
    ) # Column names
  
  d[,1] <- read.fwf(
    textConnection(d[,1]),
    widths = c(nchar(d[,1]) - 1, 1),
    colClasses = "character"
    )[,1] # Reduce "B" from Market Cap
  
  d # Display
}
finviz.sectors.table() # Test
