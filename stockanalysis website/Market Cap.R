library("rvest")

marketcap.sa <- function(x){ 
  
  L <- NULL
  
  for (n in 1:length(x)){ # Get data
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(x[n]))) %>% html_nodes('table') %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        x[n], which(x == x[n]), length(x)
      )
    )
    s <- p[grep("Market Cap", p) + 1] # Market Cap Info
    
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - 1, 1),
                  colClasses = "character")
    
    v <- as.numeric(s[1,1]) # Make data numeric
    
    s <- switch(s[1,2], "M" = v / 1000, "B" =  v, "T" = v * 1000)
    
    L <- rbind.data.frame(L, s) }
  
  rownames(L) <- x # tickers as row names
  colnames(L) <- "Market Cap" # Column name
  
  L # Display
}
marketcap.sa(c("ZIM", "AMZN")) # Test
