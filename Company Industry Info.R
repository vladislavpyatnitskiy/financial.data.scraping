info.yahoo <- function(x){ # Function to get info about company type
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/profile?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  yahoo.header1 <- tab11 %>% html_nodes('p') %>% html_nodes('span') %>%
    html_text()
  
  yahoo.header1[4] # Display
}
info.yahoo("M") # Test
