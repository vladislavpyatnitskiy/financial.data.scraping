lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

statistics.yahoo <- function(x, ts = T){ # Statistics for Stocks
  
  DF <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ 
  
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/%s/",
                            x[n], "key-statistics"),
                    add_headers(`User-Agent` = B))
    
    p <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('table') %>% .[[1]] 
    
    i <- p %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
    h <- p %>% html_nodes('thead') %>% html_nodes('th') %>% html_text()
    
    
    if (ts){
    
      D <- cbind.data.frame(i[seq(from = 1, to = length(i), by = 7)],
                            i[seq(from = 2, to = length(i), by = 7)],
                            i[seq(from = 3, to = length(i), by = 7)],
                            i[seq(from = 4, to = length(i), by = 7)],
                            i[seq(from = 5, to = length(i), by = 7)],
                            i[seq(from = 6, to = length(i), by = 7)],
                            i[seq(from = 7, to = length(i), by = 7)])
      
      colnames(D) <- h 
      
      DF <- list(DF, D) } else {
        
        D <- cbind.data.frame(i[seq(from = 1, to = length(i), by = 7)],
                              i[seq(from = 2, to = length(i), by = 7)])
        
        colnames(D) <- c("", x[n]) 
        
        rownames(D) <- D[,1] # Assign row names
        
        D <- subset(D, select = -c(1)) 
        
        if (is.null(DF)){ DF <- D } else { DF <- cbind(DF, D) } }
  }
  DF
}
statistics.yahoo(c("AAPL"), ts=F) # Test
