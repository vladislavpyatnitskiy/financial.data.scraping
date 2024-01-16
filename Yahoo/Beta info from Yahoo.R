lapply(c("quantmod","timeSeries","rvest"),require,character.only=T) # Libraries 

beta.yahoo <- function(x){ # Function to get info about company beta
  
  df <- NULL # Create list to contain values
  
  for (n in 1:length(x)){ v <- x[n] # For every ticker get beta value
  
    p <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",v,v)
    
    page.p <- read_html(p) # Read HTML & extract necessary info
    
    price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab
    
    i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    b <- as.numeric(i[grep("Beta ", i) + 1]) # Select Beta value
    
    if (is.na(b)){ l <- c(v, "^GSPC") # When Beta is not available
    
      b <- NULL #
      
      for (m in l){ b <- cbind(b, getSymbols(m, from=as.Date(Sys.Date())-365*5,
                                             to=Sys.Date(), src="yahoo",
                                             auto.assign=F)[,4])}
      
        b <- b[apply(b, 1, function(x) all(!is.na(x))),] # Get rid of NA
        
        b = diff(log(as.timeSeries(b)))[-1,] # Calculate Returns
        
        b <- as.numeric(apply(b[,1], 2,
                              function(col) ((lm((col) ~
                                                   b[,2]))$coefficients[2]))) }
        
    df <- rbind(df, round(b, 2)) } # Join betas
    
  rownames(df) <- x # Assign row names
  colnames(df) <- "Beta 5Y" # Assign column names
  
  df # Display
}
beta.yahoo(x = c("M", "X", "C", "AAPL")) # Test
