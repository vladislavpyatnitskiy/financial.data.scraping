library(rvest) # Library

sa.sector <- function(x, agg = F){ # Sector data from stockanalytics website
  
  L <- NULL
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 
    
    L <- rbind.data.frame(L, p[grep("sector", p)] %>% html_text()) } 
    
  rownames(L) <- x # tickers
  colnames(L) <- "Sector" # Column name
  
  if (agg){ # If you want to know companies belong to each industry
    
    df <- data.frame(
      Ticker = rownames(L), Sector = L$Sector, stringsAsFactors = F)
    
    L <- aggregate(Ticker ~ Sector, data = df,
                   FUN = function(x) paste(x, collapse = ", ")) }
  
  L # Display
}
sa.sector(c("STLA", "X"), T) # Test
