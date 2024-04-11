library("rvest") # Library

smartlab.ticker <- function(x, y){ # Fundamentals 
  
  l <- NULL # Store data here
  
  for (j in 1:length(x)){ k <- NULL
    
    for (m in 1:length(y)){ v <- y[m] # Fundamental
  
      s <- read_html(sprintf("https://smart-lab.ru/q/%s/?field=%s",
                             "shares_fundamental", v))
      
      s.yahoo <- s %>% html_nodes('table') %>% .[[1]] -> tab
      
      i <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
      df <- cbind(x[j], i[grep(x[j], i) + 3]) # Assign value to ticker
      
      df[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', df[,2])) # Clean
      
      if (isTRUE(grepl(" ", df[,2]))){ df[,2] <- gsub(" ", "", df[,2]) }  
      
      colnames(df) <- c("Ticker", gsub("_", "/", toupper(y[m]))) # Column names
      
      if (is.null(k)){ k<-df } else { k<-merge(x=k,y=df,by="Ticker",all=T) } }
      
    if (is.null(l)){ l <- k } else { l <- rbind(l, k) } }   # Join
      
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  rownames(l) <- x # Assign tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  return(l) # Display
}
smartlab.ticker(c("LKOH", "MAGN", "UPRO"), c("market_cap", "p_e", "p_s"))
