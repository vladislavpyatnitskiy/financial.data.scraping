library("rvest") # Library

smartlab.ratios <- function(x){ # Function to get info from Smartlab
  
  l <- NULL # Store data here
  
  for (m in 1:length(x)){ v <- x[m] # For each ratio get Smartlab HTML
    
    y <- read_html(sprintf("https://smart-lab.ru/q/%s/?field=%s",
                           "shares_fundamental",v)) %>% html_nodes('table') %>%
      .[[1]] %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    D <- NULL # Variable for Table with Name, Ticker and values
    
    for (n in 0:(length(y)/6)){ D <- rbind(D, cbind(y[(3+n*6)],y[(6+n*6)])) }
    
    D <- D[-nrow(D),] # Reduce last row
    D[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', D[,2]))
    
    for (n in 1:length(D)){ if (isTRUE(grepl(" ", D[n]))){
      
        D[n] <- gsub(" ","",D[n]) } } # Reduce gap in market cap
    
    colnames(D) <- c("Ticker", gsub("_", "/", toupper(x[m]))) # Column names
    
    if (is.null(l)){ l <- D } else { l <- merge(x=l,y=D,by="Ticker",all=T) } } 
    
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  return(l) # Display
}
smartlab.ratios(x=c("market_cap","p_e","p_bv","p_s","ev_ebitda","debt_ebitda"))
