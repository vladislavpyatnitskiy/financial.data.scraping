library("rvest")

smartlab.values <- function(){ # Current prices for ticker from Smart-Lab
  
  f <- read_html("https://smart-lab.ru/q/shares/") %>% html_nodes('table') %>% 
    html_nodes('tr') %>% html_nodes('td') # Retrieve data
  
  l <- NULL
  
  for (n in 1:length(f)){ # Derive ticker and price data

    if (
      isTRUE(f[n] %>% html_attr('class') == "trades-table__ticker") |
      isTRUE(f[n] %>% html_attr('class') == "trades-table__price")
      ){

      l <- c(l, f[n] %>% html_text())
    }
  }
  
  D <- data.frame(
    l[seq(from = 1, to = length(l), by = 2)],
    l[seq(from = 2, to = length(l), by = 2)]
  ) # organise into data frame
  
  colnames(D) <- c("Ticker", "Prices") # Assign column names
  
  ticker_names <- D[,1] # Assign tickers to new variable
  
  D <- subset(D, select = -c(1)) # Reduce excess column
  
  rownames(D) <- ticker_names # Assign tickers as row names
  
  for (n in 1:ncol(D)){ D[,n] <- round(as.numeric(D[,n]), 2) } # Numeric
  
  D # Display
}
smartlab.values() # Display
