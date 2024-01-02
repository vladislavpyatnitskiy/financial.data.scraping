library("rvest") # Library

smartlab.ratios <- function(x, l){ # Function to get info from Smartlab
  
  s <- read_html(sprintf("https://smart-lab.ru/q/shares_fundamental/?field=%s",
                         x)) # Smartlab HTML
  
  s.yahoo <- s %>% html_nodes('table') %>% .[[1]] -> tab
  
  y <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df <- NULL # Variable name for values
  
  for (n in 0:(length(y) / 6)){ # Table with Name, Ticker and values
    
    df <- rbind(df, cbind(y[(1 + n * 6)], y[(2 + n * 6)], y[(3 + n * 6)],
                          as.numeric(gsub("\\D", "", y[(6 + n * 6)]))))}
  
  df <- subset(df, select = -c(1)) # Reduce first column
  
  df <- df[-nrow(df),] # Reduce last row
  
  colnames(df) <- c("Название", "Тикер", l) # Column names
  
  df # Display
}
View(smartlab.ratios("market_cap", "Рыночная Капитализация")) # Test
