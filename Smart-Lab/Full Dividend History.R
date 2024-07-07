library("rvest") # Library

rus.dividends.full <- function(x){ # Get Full Dividend History Since 2014
  
  D <- NULL # Data Frame wtih Tickers, Dates and Dividend Amount
  
  y <- seq("2023", from = "2014", by = 1) # Dates
  
  for (m in 1:length(y)){ # Get data for each year
    
    f <- read_html(sprintf("%s%s", x, y[m])) %>% html_nodes('table') %>%
      .[[1]] %>% html_nodes('tr') # Table
    
    l <- NULL # Show only Approved Dividends
    
    for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                       "dividend_approved")){
      
        l <- c(l, f[n] %>% html_nodes('td') %>% html_text()) } }
      
    for (n in 0:(length(l) / 11)){ # Data Frame with Ticker, Date and Dividend
      
       D <- rbind.data.frame(D, cbind(l[(2 + n * 11)], l[7 + n * 11],
                                      as.numeric(gsub(",", ".", l[4+n*11])))) } 
  }
  colnames(D) <- c("Ticker", "Date", "Div Amount in Roubles") # Column Names
  
  D <- D[apply(D, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  D[,2] <- format(strptime(D[,2], format="%d.%m.%Y"),"%Y-%m-%d") # Dates format
  
  D # Display
}
rus.dividends.full("https://smart-lab.ru/dividends/index?year=")
