library("rvest") # Library

finviz.sectors.table <- function(x){ # Data Frame with info about sectors
  
  s <- read_html(sprintf("https://finviz.com/%s", x))
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[8]] -> tab # Assign Table 
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  d <- NULL # Data Frame with values
  
  for (n in 0:(length(y) / 15)){ # Market Cap & PE & FPE & PS & PB & PC & PFCF
    
    d <- rbind(d,cbind(y[(2+n*15)],y[(3+n*15)],y[(4+n*15)],y[(5+n*15)],
                       y[(7+n*15)],y[(8+n*15)],y[(9+n*15)],y[(10+n*15)])) }
  
  d <- d[-nrow(d),] # Reduce last column
  
  rownames(d) <- d[,1] # Assign row names
  
  d <- subset(d, select = -c(1)) # Reduce excessive column
  
  colnames(d) <- c("Market Cap","P/E","Forward P/E","P/S","P/B","P/C","P/FCF") 
  
  d # Display
}
finviz.sectors.table("groups.ashx?g=sector&v=120&o=name") # Test
