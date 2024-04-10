library("rvest") # Library

smartlab.ratios <- function(x){ # Function to get info from Smartlab
  
  l <- NULL # Store data here
  
  for (m in 1:length(x)){ v <- x[m] # For each ratio get Smartlab HTML
  
    s<-read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         v))
  
    s.yahoo <- s %>% html_nodes('table') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df <- NULL # Variable for Table with Name, Ticker and values
    
    for (n in 0:(length(y)/6)){ df <- rbind(df, cbind(y[(3+n*6)],y[(6+n*6)])) }
    
    df <- df[-nrow(df),] # Reduce last row
    df[,2] <- gsub('["\n"]', '', gsub('["\t"]', '', df[,2]))
    
    for (n in 1:length(df)){ if (isTRUE(grepl(" ", df[n]))){
      
        df[n] <- gsub(" ", "", df[n]) } } # Reduce gap in market cap
    
    colnames(df) <- c("Ticker", gsub("_", "/", toupper(x[m]))) # Column names
    
    # Join
    if (is.null(l)){ l<-df } else { l<-merge(x=l,y=df,by="Ticker",all=T) } } 
    
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers
  
  for (n in 1:ncol(l)){ l[,n] <- as.numeric(l[,n]) } # Make data numeric
  
  return(l) # Display
}
smartlab.ratios(x=c("market_cap","p_e","p_bv","p_s","ev_ebitda","debt_ebitda"))
