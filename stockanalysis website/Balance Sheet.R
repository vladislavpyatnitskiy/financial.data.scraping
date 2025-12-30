library(rvest) # Library

balance.sheet.sa <- function(x, sort = T){ # Balance Sheet 
  
  p <- read_html(sprintf("https://stockanalysis.com/stocks/%s/financials/%s/",
                         tolower(x), "balance-sheet")) %>%
    html_nodes('body') %>% html_nodes('table') %>% html_nodes('tr') 
  
  R <- p %>% html_nodes('td') %>% html_text() # Main data
  C <- p %>% html_nodes('th') %>% html_text() # Titles (Column names)
  
  C <- unlist(strsplit(gsub('["\n"]', '', gsub('["\t"]', '', C[1:7])), " "))
  
  D <- data.frame(
    R[seq(from = 2, to = length(R), by = 7)], # Previous Year
    R[seq(from = 3, to = length(R), by = 7)], # -1
    R[seq(from = 4, to = length(R), by = 7)], # -2
    R[seq(from = 5, to = length(R), by = 7)], # -3
    R[seq(from = 6, to = length(R), by = 7)] # -4
    )
  
  rownames(D) <- R[seq(from = 1, to = length(R), by = 7)] # Row names
  colnames(D) <- C[grep("FY", C) + 1] # Column names
  
  if (sort){ D[,sort(colnames(D))] } else { D } # Display
}
balance.sheet.sa("AAPL", F) # Test
