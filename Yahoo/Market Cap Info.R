library("rvest") # Library

c.marketcap <- function(x, caplevel = F){ # Market Cap Info
  
  df <- NULL
  
  for (n in 1:length(x)){ v <- x[n] # Subset ticker
  
    p <- read_html(sprintf("https://uk.finance.yahoo.com/quote/%s/%s", v,
                           "key-statistics"))
  
    price.yahoo1 <- p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    s <- i[grep("Market cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    if (s[1,2] == "M"){ s <- as.numeric(s[1,1])/1000 } else if (s[1,2] == "T"){
      
      s <- as.numeric(s[1,1]) * 1000 } else { s <- as.numeric(s[1,1]) }
    
    if (isTRUE(caplevel)){ # Assign Market Cap Levels for each company
      
      if (s < .3){ l <- "Micro-Cap" } # if < $300 million => Micro-Cap
      
      else if (s > .3 && s < 2) { l <- "Small-Cap" } # Small-Cap
      
      else if (s > 2 && s < 10) { l <- "Mid-Cap" } # Mid-Cap
      
      else if (s > 10 && s < 200) { l <- "Large-Cap" } # Large-Cap
      
      else { l <- "Mega-Cap" } # if > $200 billion => Mega-Cap
      
      df <- rbind.data.frame(df, cbind(l, s)) } else { # Level & Market Cap 
        
        df <- rbind.data.frame(df, s) } } # Data Frame with Market Cap only
    
    rownames(df) <- x # Tickers
    
  if (isTRUE(caplevel)){ colnames(df) <- c("Level","Marker Cap ($billions)") }
    
  else { colnames(df) <- "Marker Cap ($billions)" }
   
  df # Display
}
c.marketcap(c("AAPL", "AIG", "SWBI"), caplevel = T) # Test
