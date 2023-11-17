# Function to Scrape Statistics for Security
stat.data.yahoo <- function(x){ # Security URL
  
  s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s", x, x)
  
  s.page <- read_html(s) # Read HTML of page
  
  s.yahoo <- s.page %>% html_nodes('table') %>% .[[2]] -> tab1 # Assign Table 
  
  s.header <- tab1 %>% html_nodes('tr') %>% html_text() # Transform into Text
  
  s.header # Display
}
stat.data.yahoo("MSFT") # Test
