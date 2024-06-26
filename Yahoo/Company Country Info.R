library("rvest") # Library

c.country <- function(x){ l <- NULL # Create list

  for (n in 1:length(x)){ s <- x[n] # For each security find sector
  
    p <- sprintf("https://uk.finance.yahoo.com/quote/%s/profile?p=%s", s, s)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('p') # Find country character in elements
    
    l <- rbind(l, strsplit(toString(y),
                           "<br>")[[1]][length(strsplit(toString(y),
                                                        "<br>")[[1]])-4]) }
  colnames(l) <- "Country"
  rownames(l) <- x
  
  l # Display
}
c.country(c("STLA", "ZIM", "AMZN", "VALE")) # Test
