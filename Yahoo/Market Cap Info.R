lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

c.marketcap <- function(x, caplevel = F){ # Market Cap info via string or table
  
  j <- list(list(10, 200, "Large-Cap Companies:", "Large-Cap"), # > 10 & < 200
            list(2, 10, "Medium-Cap Companies:", "Medium-Cap"), # > 2 & < 10
            list(0.3, 2, "Small-Cap Companies:", "Small-Cap"), # > 0.3 & < 2
            list(0, 0.3, "Micro-Cap Companies:", "Micro-Cap")) # > 0 & < 0.3
  
  df <- NULL # Data Frame for Market Cap Levels and Values
  
  for (n in 1:length(x)){ # Read HTML & extract necessary info
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/",
                            x[n], "key-statistics"),
                    add_headers(`User-Agent` = B))
    
    i <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
      html_nodes('td') %>% html_text()
    
    s <- i[grep("Market cap", i) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 2, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    s <- switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000)
    
    if (isFALSE(caplevel)){ for (n in 1:length(j)){ # Market Cap Levels
      
        if (s > j[[n]][[1]] && s < j[[n]][[2]]){ l <- j[[n]][[4]] 
        
        } else if (s > 200){ l <- "Mega-Cap" } else { next } } 
      
      # Market Cap Level with values OR Market Cap values only
      df <- rbind.data.frame(df, cbind(l, s)) } else { df <- rbind(df, s) } }
  
  if (isFALSE(caplevel)){ # Create Data Frame
    
    rownames(df) <- x # Tickers
    colnames(df) <- c("Level", "Marker Cap ($billions)") # column names
    
    df } else { c <- as.numeric(df) # Make available values to numeric format
    
    names(c) <- x # Assign names to them
    
    c <- sort(c, decreasing = T) # Sort in a descending way
    
    m <- NULL # Write advices about securities according to Market Cap
    
    if (isFALSE(identical(names(which(c > 200)), character(0)))){
      
      m <- c(m, paste("Mega-Cap Companies:", toString(names(which(c > 200)))))}
    
    for (n in 1:length(j)){ #
      
      if (isFALSE(identical(names(which(c > j[[n]][[1]] & c < j[[n]][[2]])),
                            character(0)))){
        m <- c(m,
               paste(j[[n]][[3]],
                     toString(names(which(c>j[[n]][[1]] & c<j[[n]][[2]])))))} }
    m } # Display
}
c.marketcap(c("AAPL", "AIG", "SWBI", "NRG"), caplevel = F) # Test
