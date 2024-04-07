library("rvest")

rus.div <- function(x, y){ # Dividends of Russian Stocks
  
  L <- NULL
  
  for (m in 1:length(y)){ a <- y[m] # Get data for each year
  
    p <- read_html(sprintf("https://smart-lab.ru/dividends/index?year=%s", a))
    
    div <- p %>% html_nodes('table') %>% .[[1]] -> tab
    
    f <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df <- NULL # Variable for Table with Name, Ticker and values
    
    if (isTRUE(any(f == x))){ # Format info to Data Frame
      
      for (n in 0:(length(f) / 11)){ if (isTRUE(f[(2 + n * 11)] == x)){
        
        df <- rbind.data.frame(df,
                               cbind(f[7 + n * 11],
                                     as.numeric(gsub(",", ".",
                                                     f[4 + n * 11])))) } } }
    else { next } # Next Year
    
    ts <- df[,1] # Dates
    
    for (n in 1:length(ts)){ # Reform data format
      
      Y <- as.character(read.fwf(textConnection(ts[n]),
                                 widths=c(nchar(ts[n]) - 4, nchar(ts[n]) - 0),
                                 colClasses = "character")[2]) # Year
      
      M <- as.character(read.fwf(textConnection(ts[n]),
                                 widths=c(nchar(ts[n]) - 7, nchar(ts[n]) - 8),
                                 colClasses = "character")[2]) # Month
      
      d <- as.character(read.fwf(textConnection(ts[n]),
                                 widths=c(nchar(ts[n]) - 8, nchar(ts[n]) - 10),
                                 colClasses = "character")[1]) # Day
      
      ts[n] <- paste(Y, M, d, sep = "-") } # Concatenate dates
    
    df <- as.data.frame(df[,-1]) # Reduce excessive column
    
    rownames(df) <- ts # Dates
    colnames(df) <- x 
    
    L <- rbind(L, df) } # Tickers
  
  L # Display
}
rus.div("PHOR", seq("2023", from = "2014", by = 1)) # Test
