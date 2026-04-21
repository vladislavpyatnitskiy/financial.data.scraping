library("rvest") # Library

dowjones.list.wiki <- function(yahoo=T){ # Show list of tickers from DJIA
  
  D <- read_html(
    "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average") |> 
    html_element("#constituents") %>% html_nodes('tr') 
  
  dow_values <- gsub("[\n]", "", D %>% html_nodes('td') %>% html_text())
  dow_names <- gsub("[\n]", "", D %>% html_nodes('th') %>% html_text())
  
  tickers <- dow_values[seq(from = 2, to = length(dow_values), by = 6)]
  
  if (yahoo) return(tickers) # When you need only tickers
  
  df <- data.frame(
    tickers, # Tickers
    dow_names[which(dow_names == "3M"):length(dow_names)], # Company Names
    dow_values[seq(from = 3, to = length(dow_values), by = 6)] # Sector
  ) 
  
  colnames(df) <- c( "Ticker", "Company Name", "Sector")
  
  df # Display
}
dowjones.list.wiki(F) # Test
