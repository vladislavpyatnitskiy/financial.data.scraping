library(rvest) # Library

sa.industry <- function(x, agg = F){ # Get sector info for portfolio stocks
  
  L <- NULL # Create list
  
  for (n in 1:length(x)){ y <- x[n] # Get data
  
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 

    message(
      sprintf(
        "%s industry data is downloaded (%s from %s)",
        y, which(x == y), length(x)
      )
    )
                         
    L <- rbind.data.frame(L, p[grep("industry", p)] %>% html_text()) } 

  colnames(L) <- "Industry"
  rownames(L) <- x # tickers
  
  if (agg){ # If you want to know companies belong to each industry
    
    df <- data.frame(Ticker=rownames(L),Industry=L$Industry,stringsAsFactors=F)
    
    L <- aggregate(Ticker ~ Industry, data = df,
                   FUN = function(x) paste(x, collapse = ", ")) }
  L # Display
}
sa.industry(c("STLA", "X"), T) # Test
