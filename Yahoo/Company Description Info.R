lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

c.description <- function(x){ # Description info for securities
  
  l <- NULL # Get Company Info

  for (n in 1:length(x)){ s <- x[n]
  
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile/",
                            s), add_headers(`User-Agent` = B))
    
    y <- read_html(response) %>% html_nodes('main') %>%
      html_nodes('section') %>% html_nodes('section') %>%
      html_nodes('article') %>% html_nodes('section') %>%
      html_nodes('section') %>% html_nodes('p') %>% html_text()
    
    l <- c(l, y) } # Add description to list
  
  l # Display
}
c.description(c("AAPL", "X", "C")) # Test
