library("rvest") # Library

isin.investing.com <- function(x){ # ISIN Numbers for Russian Stocks
  
  l <- NULL 
  
  for (n in 1:length(x)){ i <- x[n]
    
    p <- read_html(sprintf("https://ru.investing.com/equities/%s_rts", i))
    
    price.yahoo1 <- p %>% html_nodes('dl') %>% .[[2]] -> tab
    
    B <- tab %>% html_nodes('div') %>% html_nodes('dd') %>% html_text()
    V <- tab %>% html_nodes('div') %>% html_nodes('dt') %>% html_text()
    
    l <- rbind(l, gsub(" ", "", B[10])) }
    
  colnames(l) <- V[10] # ISIN Numbers
  rownames(l) <- x # Stock names
  
  l
}
isin.investing.com(c("lukoil", "novatek")) # Test
