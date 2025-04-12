lapply(c("quantmod", "timeSeries", "rvest", "httr", "xml2"), require,
       character.only = T) # Libraries 

beta.yahoo <- function(x){ # Function to get info about company beta
  
  df <- NULL # Create list to contain values
  
  for (n in 1:length(x)){ v <- x[n] # For every ticker get beta value
  
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/", v,
                            "key-statistics"), add_headers(`User-Agent` = B))
    
    y <- read_html(response) %>% html_nodes('table') %>% .[[8]] %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text() 
    
    b <- as.numeric(y[grep("Beta ", y) + 1]) # Select Beta value
    
    if (is.na(b)){ l <- c(v, "^GSPC") # When Beta is not available
      
      b <- NULL # Get Stock Price Data and calculate Beta yourself
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-365*5,
                                             to=Sys.Date(), src="yahoo",
                                             auto.assign=F)[,4])}
      
      b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
      
      b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns and Beta
      
      b <- as.numeric(apply(b[,1], 2,
                            function(col) ((lm((col) ~
                                                 b[,2]))$coefficients[2]))) }
    df <- rbind(df, round(b, 2)) } # Join betas
    
  rownames(df) <- x # Assign row names
  colnames(df) <- "Beta 5Y" # Assign column names
  
  df # Display
}
beta.yahoo(x = c("M", "X", "C", "AAPL")) # Test
