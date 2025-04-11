lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

c.country <- function(x){ # Get country info for securities
  
  l <- NULL
  
  for (n in 1:length(x)){ s <- x[n]
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile/",
                            s), add_headers(`User-Agent` = B))
  
    y <- read_html(response) %>% html_nodes('main') %>%
      html_nodes('section') %>% html_nodes('section') %>%
      html_nodes('article') %>% html_nodes('section') %>%
      html_nodes('section') %>% html_nodes('div') %>% html_nodes('div') %>%
      html_text()
    
    l <- rbind(l, y[grep("Sector", y) - 1]) } # Join 
    
  colnames(l) <- "Country"
  rownames(l) <- x

  l # Display
}
c.country(c("AMZN", "VALE", "STLA", "ZIM")) # Test
