library("rvest") # library

rtsi.list.wiki <- function(yahoo=T){ # Tickers from RTS Index
  
  f <- read_html("https://ru.wikipedia.org/wiki/Индекс_РТС") %>% 
    html_nodes('table')
  
  l <- NULL

  for (n in 1:length(f)){ # Derive ticker and price data

    if (isTRUE(f[n] %>% html_attr('class') == "wikitable sortable")){

      l <- c(l, f[n])
    }
  }
  D <- l %>% .[[1]] %>% html_nodes('tbody') %>% html_nodes('tr') %>% 
    html_nodes('td') %>% html_text()
  
  df <- gsub("[\n]", "", D)
  
  tickers <- df[seq(from = 2, to = length(df), by = 7)]
  
  if (yahoo) return(tickers) # When you need only tickers
  
  pct <- gsub("%", "", gsub(",", ".", df[seq(from=7, to=length(df), by=7)]))
  
  df <- data.frame(
    tickers, # Tickers
    gsub("\\", "", df[seq(from=3, to=length(df), by=7)], fixed=T),
    pct # Weight
  )
  
  colnames(df) <- c( "Ticker", "Company Name", "Weight (%)")
  
  df # Display
}
rtsi.list.wiki(T) # Test
