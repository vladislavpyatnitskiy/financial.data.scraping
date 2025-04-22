lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

c.executives <- function(x){ # Get info about executives
  
  B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
             "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
             sep = " ")
  
  response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile",
                          x), add_headers(`User-Agent` = B))
  
  p <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
    html_nodes('table') %>% .[[1]] %>% html_nodes('tr') %>%
    html_nodes('td') %>% html_text()
  
  D <- cbind.data.frame(p[seq(from = 1, to = length(p), by = 5)],
                        p[seq(from = 2, to = length(p), by = 5)],
                        p[seq(from = 3, to = length(p), by = 5)],
                        p[seq(from = 4, to = length(p), by = 5)],
                        p[seq(from = 5, to = length(p), by = 5)])
  
  colnames(D) <- c("Name", "Title", "Pay", "Exercised", "Year Born")
  
  D # Output
}
c.executives("AAPL") # Test
