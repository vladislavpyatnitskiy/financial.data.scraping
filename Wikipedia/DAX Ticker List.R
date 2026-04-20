library("rvest")

dax.list.wiki <- function(yahoo=F){ # List of tickers from German Index DAX
  
  f <- dax_html |> 
    html_element("#constituents") %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_text() # Get data
  
  tickers <- f[seq(from = 1, to = length(f), by = 7)]
    
  if (yahoo) return(tickers) # When you need only tickers
  
  df <- data.frame(
    tickers, # Tickers
    f[seq(from = 3, to = length(f), by = 7)], # Company Names
    f[seq(from = 4, to = length(f), by = 7)] # Sector
  ) 
  
  colnames(df) <- c( "Ticker", "Company Name", "Sector")
  
  df # Display
}
dax.list.wiki(F) # Test
