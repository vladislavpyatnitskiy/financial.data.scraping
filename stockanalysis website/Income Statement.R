library(rvest) # Library

income.statement.sa <- function(x, sort = T){ # Income Statement 
  
  p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/financials/",
                         tolower(x))) %>% html_nodes('table') %>%
    html_nodes('tr')
  
  R <- p %>% html_nodes('td') %>% html_text() # Main data
  C <- p %>% html_nodes('th') %>% html_text() # Titles (Column names)
  
  C <- unlist(strsplit(gsub('["\n"]', '', gsub('["\t"]', '', C[2:6])), " "))
  
  D <- data.frame(R[seq(from = 2, to = length(R), by = 7)], # Previous Year
                  R[seq(from = 3, to = length(R), by = 7)], # -1
                  R[seq(from = 4, to = length(R), by = 7)], # -2
                  R[seq(from = 5, to = length(R), by = 7)], # -3
                  R[seq(from = 6, to = length(R), by = 7)]) # -4
  
  rownames(D) <- R[seq(from = 1, to = length(R), by = 7)] # Row names
  colnames(D) <- C[seq(from = 2, to = length(C), by = 2)] # Column names
  
  if (isTRUE(sort)){ D[,sort(colnames(D))] } else { D } # Display
}
income.statement.sa("ZIM") # Test
