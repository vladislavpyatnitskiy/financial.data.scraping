library("rvest") # Library

cac40.list.wiki <- function(yahoo=F){ # Tickers from CAC 40
   
  f <- read_html("https://en.wikipedia.org/wiki/CAC_40") |> 
    html_element("#constituents") %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_text() # Get data
  
  tickers <- gsub("\n", "", f[seq(from=4, to=length(f), by=4)])
  
  if (yahoo) return(tickers) # When you need only tickers
  
  df <- data.frame(
    f[seq(from = 1, to = length(f), by = 4)], # Company Names
    f[seq(from = 2, to = length(f), by = 4)], # Sector
    f[seq(from = 3, to = length(f), by = 4)], # Industry
    tickers # Tickers
  ) 
   
  colnames(df) <- c("Company Name", "Sector", "Industry", "Ticker")
   
  df # Display
}
cac40.list.wiki(T) # Test 
