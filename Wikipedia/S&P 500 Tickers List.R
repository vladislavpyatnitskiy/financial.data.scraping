library("rvest") # Library

sp500.list.wiki <- function(yahoo=T){ # Tickers from S&P 500
  
  f <- sp500_html |> 
    html_element("#constituents") %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_text() # Get data
  
  tickers <- gsub("[\n]", "", f[seq(from = 1, to = length(f), by = 8)])
  
  if (yahoo) return(tickers) # When you need only tickers
  
  df <- data.frame(
    tickers, # Tickers
    f[seq(from = 2, to = length(f), by = 8)], # Company Names
    f[seq(from = 3, to = length(f), by = 8)], # Sector
    f[seq(from = 4, to = length(f), by = 8)] # Industry
  )
  
  colnames(df) <- c( "Ticker", "Company Name", "Sector", "Industry")
  
  df # Display
}
sp500.list.wiki(T) # Test
