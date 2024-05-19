library("rvest")

rus.dividends <- function(x, s = NULL, e = NULL){ # Dividends of Russian Stocks
  
  P <- NULL #
  
  y <- seq("2023", from = "2014", by = 1) # Dates
  
  for (j in 1:length(x)){ L <- NULL # Data for each security
  
    v <- x[j] # One by one
    
    for (m in 1:length(y)){ a <- y[m] # Get data for each year
      
      p <- read_html(sprintf("https://smart-lab.ru/dividends/index?year=%s",a))
      
      div <- p %>% html_nodes('table') %>% .[[1]] -> tab # Table
      
      f <- tab %>% html_nodes('tr') # Show rows of dividends
      
      l <- NULL # Show only approved dividends 
      
      for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                         "dividend_approved")){
        
          q <- f[n] %>% html_nodes('td') %>% html_text()
          
          l <- c(l, q) } } # Join rows of approved dividends
      
      df <- NULL # Variable for Table with Name, Ticker and values
      
      if (isTRUE(any(l == v))){ # Format info to Data Frame
        
        for (n in 0:(length(l) / 11)){ if (isTRUE(l[(2 + n * 11)] == v)){
          
            df <- rbind.data.frame(df,
                                   cbind(l[7 + n * 11],
                                         as.numeric(gsub(",", ".",
                                                         l[4+n*11])))) } } }
      else { next } # Next Year
      
    ts <- df[,1] # Dates
    
    for (n in 1:length(ts)){ # Reform data format
      
      Y<-as.character(read.fwf(textConnection(ts[n]),widths=c(nchar(ts[n])-4,
                                                              nchar(ts[n])-0),
                               colClasses = "character")[2]) # Year
      
      M<-as.character(read.fwf(textConnection(ts[n]),widths=c(nchar(ts[n])-7,
                                                              nchar(ts[n])-8),
                               colClasses = "character")[2]) # Month
      
      d<-as.character(read.fwf(textConnection(ts[n]),widths=c(nchar(ts[n])-8,
                                                              nchar(ts[n])-10),
                                 colClasses = "character")[1]) # Day
      
      ts[n] <- paste(Y, M, d, sep = "-") } # Concatenate dates
    
    df <- as.data.frame(df[,-1]) # Reduce excessive column
    
    df <- data.frame(ts, df) # Join time series and dividends
    
    colnames(df) <- c("Date", v) # Column names
    
    L <- rbind(L, df) } # Join rows of data frames
    
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
  
  if (is.null(s) && is.null(e)){ P } # When dates are not submitted 
  
  else if (is.null(s)){ P[rownames(P) < e,] } # When only End Date submitted
  
  else if (is.null(e)){ P[rownames(P) > s,] } # When only Start Date submitted
  
  else { P[rownames(P) < e & rownames(P) > s,] } # When both dates submitted
}
rus.dividends(c("LKOH", "MAGN", "PHOR", "MGNT"), s = "2022-01-01") # Test
