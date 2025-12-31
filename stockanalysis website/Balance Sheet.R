library(rvest) # Library

balance.sheet.sa <- function(x, sort = T){ # Data for Balance Sheet 
  
  L <- NULL 
  
  for (n in 1:length(x)){ i = x[n] # Ticker
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/financials/%s/",
                           tolower(i), "balance-sheet")) %>%
      html_nodes('body') %>% html_nodes('table') %>% html_nodes('tr') 

    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        i, which(x == i), length(x)
      )
    )
                         
    R <- p %>% html_nodes('td') %>% html_text() # Main data
    C <- p %>% html_nodes('th') %>% html_text() # Titles (Column names)
    
    C <- unlist(strsplit(gsub('["\n"]', '', gsub('["\t"]', '', C[1:7])), " "))
    
    if (isTRUE("TTM" %in% C)){ C <- c("TTM", C[grep("FY", C) + 1]) }
    
    else { C <- C[grep("FY", C) + 1] } # Titles (Column names)
    
    vals <- na.omit(c(R[1], R[grep("Upgrade", R) + 1])) # Financial Data
    
    vals <- vals[!is.na(vals)] # Reduce NA
    
    R <- R[-which(R == "Upgrade ")] # Reduce upgrade
    
    vals2 <- c(which(R %in% vals), length(R) + 1) # See positions of Data
    
    l <- NULL # Extract values for each financial position
    
    for (m in 1:(length(vals2) - 1)){ # Values for each year
      
      ff <- vals2[m] + 1 # Value for first year 
      fe <- vals2[m+1] - 1 # Value for last year 
      
      l <- rbind.data.frame(l, R[ff:fe]) } # Values for all years
    
    rownames(l) <- vals # Assign financial positions as row names
    colnames(l) <- C # Assign years as column names
    
    if (sort){ l <- l[,sort(seq(ncol(l)), decreasing = T)] } # Sort
    
    if (is.null(L)){ L <- list(l) } else { L[[n]] <- l } } # Add to list
  
  names(L) <- x # Assign tickers for lists as names
  
  L # Display
}
balance.sheet.sa(c("AMZN", "AAPL", "ZIM"), T) # Test
