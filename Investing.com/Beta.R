library("rvest") # library

beta.investing.com <- function(x){ # Beta values for Russian Stocks
  
  l <- NULL 
  
  for (n in 1:length(x)){ i <- x[n]
    
    p <- read_html(sprintf("https://ru.investing.com/equities/%s", i))
    
    price.yahoo1 <- p %>% html_nodes('dl') %>% .[[2]] -> tab
    
    B <- tab %>% html_nodes('div') %>% html_nodes('dd') %>% html_text()
    V <- tab %>% html_nodes('div') %>% html_nodes('dt') %>% html_text()
    
    l <- rbind(l, as.numeric(gsub(",", ".", B[6]))) }
    
  colnames(l) <- V[6] # Beta
  rownames(l) <- x # Stock names
  
  l
}
beta.investing.com(c("lukoil_rts", "novatek_rts")) # Test
