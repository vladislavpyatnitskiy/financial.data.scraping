library(rvest) # Library

income.statement.sa <- function(x){ # Income Statement from stockanalytics
    
  p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/financials/",
                         tolower(x))) %>% html_nodes('table') %>%
    html_nodes('tr')
  
  R <- p %>% html_nodes('td') %>% html_text() # Main data
  
  C <- p %>% html_nodes('th') %>% html_text() # Titles (Column names)
  
  C <- gsub('["\n"]', '', gsub('["\t"]', '', C[2:7])) # Clean
  
  D <- data.frame(R[seq(from = 2, to = length(R), by = 8)], # TTM
                  R[seq(from = 3, to = length(R), by = 8)], # 2023
                  R[seq(from = 4, to = length(R), by = 8)], # 2022
                  R[seq(from = 5, to = length(R), by = 8)], # 2021
                  R[seq(from = 6, to = length(R), by = 8)], # 2020
                  R[seq(from = 7, to = length(R), by = 8)]) # 2019
  
  rownames(D) <- R[seq(from = 1, to = length(R), by = 8)] # Row names
  colnames(D) <- C # Column names
  
  D # Display
}
income.statement.sa("ZIM") # Test
