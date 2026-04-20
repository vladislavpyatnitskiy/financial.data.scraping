library("rvest") # Library

ftse100.list.wiki <- function(yahoo = T){ # Tickers from FTSE 100
  
  f <- read_html("https://en.wikipedia.org/wiki/FTSE_100_Index") |> 
    html_element("#constituents") %>% html_nodes('tr') %>% html_nodes('td') %>% 
    html_text()
  
  if (yahoo) return(paste(f[seq(from=2, to=length(f), by=3)], ".L", sep=""))
  
  df <- data.frame(
    f[seq(from = 1, to = length(f), by = 3)],
    f[seq(from = 2, to = length(f), by = 3)],
    f[seq(from = 3, to = length(f), by = 3)]
  )
  
  colnames(df) <- c("Company Name", "Ticker", "Sector")
  
  df[,3] <- gsub("\n", "", df[,3])
  
  df
}
ftse100.list.wiki(F) # Test
