library("rvest") # Library

smartlab.ratios <- function(x, c){ # Function to get info from Smartlab
  
  l <- NULL #
  
  for (m in 1:length(x)){ v <- x[m] #
  
    s<-read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         v)) # Smartlab HTML
    
    s.yahoo <- s %>% html_nodes('table') %>% .[[1]] -> tab
    
    y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    df <- NULL # Variable name for values
    
    # Table with Name, Ticker and values
    for (n in 0:(length(y)/6)){ df <- rbind(df,cbind(y[(3+n*6)],y[(6+n*6)])) }
    
    df <- df[-nrow(df),] # Reduce last row
    df[,2] <- as.numeric(gsub('["\t\n"]','',df[,2])) # Reduce characters
    colnames(df) <- c("Ticker", c[m]) # Column names
    
    # Join
    if (is.null(l)){ l<-df } else { l<-merge(x=l,y=df,by="Ticker",all=T) } } 
    
  l # Display
}
View(smartlab.ratios(x = c("ev_ebitda", "p_bv", "debt_ebitda", "p_e"),
                     c = c("EV/EBITDA", "P/BV", "Debt/EBITDA", "P/E")))
