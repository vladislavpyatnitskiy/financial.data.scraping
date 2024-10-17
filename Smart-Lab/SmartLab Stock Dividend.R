library("rvest")

rus.dividends <- function(x, s = NULL, e = NULL){ # Dividends of Russian Stocks
  
  P <- NULL # Data of Dividends with Time Series and Tickers
  
  y <- seq("2023", from = "2014", by = 1) # Dates
  
  for (j in 1:length(x)){ L <- NULL # Data for each security
  
  v <- x[j] # One by one
  
  for (m in 1:length(y)){ a <- y[m] # Get data for each year
  
  p <- read_html(sprintf("https://smart-lab.ru/dividends/index?year=%s",a))
  
  f <- p %>% html_nodes('table') %>% .[[1]] %>% html_nodes('tr') # Table
  
  l <- NULL # Show only approved dividends 
  
  for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                     "dividend_approved")){
    
    l <- c(l, f[n] %>% html_nodes('td') %>% html_text()) } } 
  
  D <- NULL # Variable for Table with Name, Ticker and values
  
  if (isTRUE(any(l == v))){ # Data Frame of Dates and Dividends
    
    for (n in 0:(length(l)/11)){ if (isTRUE(l[(2 + n * 11)] == v)){
      
      D <- rbind.data.frame(D, cbind(l[7 + n * 11],
                                     as.numeric(gsub(",", ".",
                                                     l[4+n*11])))) } } }
  else { next } # Next Year
  
  ts <- D[,1] # Dates
  
  ts <- format(strptime(ts, format = "%d.%m.%Y"), "%Y-%m-%d") # Dates
  
  D <- as.data.frame(D[,-1]) # Reduce excessive column
  
  D <- data.frame(ts, D) # Join time series and dividends
  
  colnames(D) <- c("Date", v) # Column names
  
  L <- rbind(L, D) } # Join rows of data frames
  
  for (n in 1:nrow(L)){ while (isFALSE(L[n,1] == L[nrow(L),1])){
    
    if (isTRUE(L[n + 1,1] == L[n,1]) & isTRUE(L[n + 1,2] == L[n,2])){ 
      
      L <- L[-(n + 1),] } # Delete Rows with duplicates
    
    break } } # End loop when it is over
  
  if (is.null(P)){ P = L } else { P <- merge(x=P, y=L, by="Date", all=T) } }
  
  dates <- P[,1] # Put dates into separate column
  
  P <- as.data.frame(P[,-1]) # Reduce excessive column
  
  for (n in 1:length(dates)){ while (dates[n] != dates[length(dates)]){
    
    if (isTRUE(dates[n + 1] == dates[n])){ # Add 1 when Double Dividends
      
      dates[n + 1] <- paste(dates[n + 1], "1", sep = "") } #
    
    break } } # End when the loop is over
  
  rownames(P) <- dates # Dates
  
  P[is.na(P)] <- 0 # Substitute NA with Zero values
  
  if (is.null(s) && !is.null(e)){ d <- rownames(P)[rownames(P) < e] # Dates
  
  P <- P[rownames(P) < e,] } # Only End Date submitted
  
  else if (!is.null(s) && is.null(e)){ d <- rownames(P)[rownames(P)>s] # Dates
  
  P <- P[rownames(P) > s,] } # Only Start Date submitted
  
  else if (!is.null(s) && !is.null(e)){ # When both submitted
    
    d <- rownames(P)[rownames(P) > s & rownames(P) < e] # Dates
    
    P <- P[rownames(P) > s & rownames(P) < e,] }  # Dividends for this Period
  
  if (isTRUE(is.character(P))){ P <- as.data.frame.numeric(P) # Make Numeric
  
  rownames(P) <- d # Dates
  colnames(P) <- x } # Tickers
  
  if (isTRUE(ncol(P) == 1)){ colnames(P) <- x }
  
  P # Display
}
rus.dividends(c("LKOH", "NVTK"), s = "2022-01-01", e = "2024-01-01") # Test
