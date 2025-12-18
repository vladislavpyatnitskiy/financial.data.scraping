library("rvest") # Library

smartlab.ratios <- function(x){ # Function to get info from Smartlab
  
  l <- NULL # Store data here
  
  for (m in 1:length(x)){ v <- x[m] # For each ratio get Smartlab HTML
    
    y <- read_html(sprintf("https://smart-lab.ru/q/%s/?field=%s",
                           "shares_fundamental",v)) %>% html_nodes('table') %>%
      .[[1]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()

    message(sprintf("%s is downloaded", gsub("_", "/", toupper(v))))
                         
    D <- NULL # Variable for Table with Name, Ticker and values
    
    for (n in 0:(length(y)/6)){ D <- rbind(D, cbind(y[(3+n*7)],y[(6+n*7)])) }
    
    D <- D[-nrow(D),] # Reduce last row
    
    D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2]))
    
    D <- gsub(" ", "", D) # Reduce gap in market cap
    
    colnames(D) <- c("Ticker", gsub("_", "/", toupper(v))) # Column names
    
    D <- subset(D, !apply(D == "", 1, any)) # Reduce empty row
    
  if (is.null(l)){ l <- D } else { l <- merge(x=l,y=D,by="Ticker",all=T) } } 
  
  l <- l[-1,]
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  return(l) # Display
}
smartlab.ratios(x=c("market_cap", "p_e","p_bv","p_s","ev_ebitda","debt_ebitda"))
