library("rvest") # Library

smartlab.ratios <- function(x){ # Function to get info from Smartlab
  
  l <- NULL # Store data here
  
  for (m in 1:length(x)){ v <- x[m] # For each ratio get Smartlab HTML
  
    y <- read_html(
      sprintf(
        "https://smart-lab.ru/q/%s/?field=%s",
        "shares_fundamental",
        v)
      ) %>% html_nodes('table') %>% html_nodes('tr')
    
    message(sprintf("%s is downloaded", gsub("_", "/", toupper(v))))
    
    c = y %>% html_nodes('td') %>% html_nodes('strong') %>% html_text()
    
    tickers = y %>% html_nodes('td') %>% html_text()
    
    D = NULL
    
    for (n in 0:(length(tickers) / 8)) D <- rbind(D, tickers[(3 + n * 8)])
    
    c <- gsub('["\n"]', '', gsub('["\t"]', '', c))
    
    D <- cbind.data.frame(as.data.frame(D), as.data.frame(c))
    
    D <- subset(D, !apply(D == "", 1, any)) # Reduce empty row
    
    D[,2] <- gsub(" ", "", D[,2]) # Reduce gap in market cap
    
    colnames(D) <- c("Ticker", gsub("_", "/", toupper(v))) # Column names
    
    if (is.null(l)) l = D else l = merge(x = l, y = D, by = "Ticker", all=T) } 
  
  funs <- colnames(l)[2:length(colnames(l))]
    
  tickers <- l[,1] # Move tickers to row names
   
  l <- as.data.frame(l[,-1]) # Reduce excessive column with tickers

  rownames(l) <- tickers

  for (n in 1:ncol(l)) l[,n] <- as.numeric(l[,n]) # Make data numeric

  colnames(l) <- funs
  
  l <- l[-1,]
  
  return(l) # Display
}
smartlab.ratios(x=c("market_cap","p_e","p_bv","p_s","ev_ebitda","debt_ebitda"))
