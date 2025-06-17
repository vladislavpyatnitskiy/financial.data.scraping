library("rvest") # Library

smartlab.ticker <- function(x, y){ # Fundamental 
  
  l <- NULL # Store data here
  
  for (j in 1:length(x)){ k <- NULL
    
    for (m in 1:length(y)){ v <- y[m] # Fundamental
      
      i <- read_html(sprintf("https://smart-lab.ru/q/%s/?field=%s",
                             "shares_fundamental", v)) %>%
        html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
        html_nodes('td') %>% html_text()
      
      D <- NULL
      
      for (n in 0:(length(i)/6)){ D = rbind(D, cbind(i[(3+n*6)], i[(6+n*6)])) }
      
      D <- cbind(x[j], D[D[,1] == x[j], 2]) 
      
      D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2])) # Clean
      
      if (isTRUE(grepl(" ", D[,2]))){ D[,2] <- gsub(" ", "", D[,2]) }  
      
      colnames(D) <- c("Ticker", gsub("_", "/", toupper(y[m]))) # Column names
      
      if (is.null(k)){ k = D } else { k = merge(x=k,y=D,by="Ticker",all=T) } }
    
    if (is.null(l)){ l <- k } else { l <- rbind(l, k) } }   # Join
  
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  if (ncol(l) == 1){ colnames(l) <- gsub("_", "/", toupper(y)) } 
  
  rownames(l) <- x # Assign tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  return(l) # Display
}
smartlab.ticker(c("LKOH", "MAGN", "UPRO"), c("market_cap", "p_e", "p_s"))
