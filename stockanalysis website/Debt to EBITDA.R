library("rvest") # Library

debt.editda.sa <- function(x){ # Debt / EBITDA ratio
  
  L <- NULL
  
  for (n in 1:length(x)){ # Get data
    
    p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(x[n]))) %>% html_nodes('table') %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    L <- rbind.data.frame(L, as.numeric(p[grep("Debt / EBITDA ", p) + 1])) }
  
  rownames(L) <- x # tickers as row names
  colnames(L) <- "Debt/EBITDA" # Column name

  L # Display
}
debt.editda.sa(c("ZIM", "AMZN")) # Test
