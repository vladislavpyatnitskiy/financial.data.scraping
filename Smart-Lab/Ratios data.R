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
    df[,2] <- as.numeric(gsub('["\t\n"]', '', df[,2])) # Reduce characters
    colnames(df) <- c("Ticker", gsub("_", "/", toupper(x[m]))) # Column names
    
    # Join
    if (is.null(l)){ l<-df } else { l<-merge(x=l,y=df,by="Ticker",all=T) } } 
    
  if (isTRUE(l[1,1] == "")){ l <- l[-1,] } # Reduce empty row
  
  rownames(l) <- l[,1] # Move tickers to row names
  
  l <- l[,-1] # Reduce excessive column with tickers which are in row names
  
  return(l) # Display
}
smartlab.ratios(x=c("ev_ebitda","p_bv","debt_ebitda","p_e","market_cap","p_s"))
