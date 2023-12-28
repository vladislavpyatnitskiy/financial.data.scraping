c.marketcap <- function(x){ # Market Cap Info
  
  new.info <- NULL
  
  for (n in 1:length(x)){ v <- x[n] # Subset ticker
    
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s.header <- i[2] # Info about market capitalisation
    
    s.header<-read.fwf(textConnection(s.header),widths=c(nchar(s.header)-1,1),
                       colClasses = "character")
    
    if (s.header[1,2] == "M"){ s.header <- as.numeric(s.header[1,1]) / 1000 }
    
    else if (s.header[1,2] == "T"){ s.header<-as.numeric(s.header[1,1])*1000 }
    
    else s.header <- as.numeric(s.header[1,1]) # Format to billion format
    
    new.info <- rbind.data.frame(new.info, s.header) } # Data Frame 
  
  rownames(new.info) <- x # Tickers
  colnames(new.info) <- "Marker Cap ($billions)" # Column Name
  
  new.info # Display
}
c.marketcap(c("AAPL", "AMZN", "SWBI", "AIG")) # Test
