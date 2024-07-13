library("rvest") # Library

morningstar.sector <- function(x){ # Get Sector Info from Morningstar
  
  L <- NULL 
  
  for (m in 1:length(x)){ s <- tolower(x[m]) # Get HTML and clean it
  
    f <- read_html(sprintf("https://www.morningstar.com/stocks/misx/%s/quote",
                           s)) %>% html_nodes('section') %>%
      html_nodes('dd') %>% html_nodes('span') 
    
    l <- NULL 
    
    for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                       "mdc-locked-text__mdc mdc-string")){
      
      l <- c(l, f[n] %>% html_text()) } } # Final version
    
    L <- rbind.data.frame(L, l[1]) } # Join
  
  colnames(L) <- "Sector" # Assign Column name
  rownames(L) <- x # assign tickers as row names
  
  L # Display
}
morningstar.sector("LKOH") # Test
