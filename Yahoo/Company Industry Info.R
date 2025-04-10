lapply(c("rvest", "httr", "xml2"), require, character.only = T) # Libs

c.industry <- function(x){ L <- NULL #l <- NULL # Create list
  
  for (n in 1:length(x)){ s <- x[n]
    
    B <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
               "AppleWebKit/537.36", "Chrome/122.0.0.0", "Safari/537.36",
               sep = " ")
    
    response <- GET(sprintf("https://uk.finance.yahoo.com/quote/%s/profile", s),
                    add_headers(`User-Agent` = B))
    
    f <- read_html(content(response, as = "text", encoding = "UTF-8")) %>%
      html_nodes('div') %>% html_nodes('dl') %>% html_nodes('div') %>%
      html_nodes('strong') %>% html_text() %>% .[2]
      
    L <- rbind.data.frame(L, f) }
  
  colnames(L) <- "Industry" # 
  rownames(L) <- x #
  
  L # Display
}
c.industry(x = c("AAPL", "NRG", "PVH", "C")) # Test
