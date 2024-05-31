library("rvest") # Library

marketcap.investing.com <- function(x){ # Market Cap for Russian Stocks
  
  l <- NULL 
  
  for (n in 1:length(x)){ i <- x[n]
  
    p <- read_html(sprintf("https://ru.investing.com/equities/%s", i))
    
    price.yahoo1 <- p %>% html_nodes('dl') %>% .[[2]] -> tab
    
    b <- tab %>% html_nodes('div') %>% html_nodes('dd') %>% html_text()
    V <- tab %>% html_nodes('div') %>% html_nodes('dt') %>% html_text()
    
    b <- b[1] # Scrape value, reduce "B" and change to numeric type
    
    B <- read.fwf(textConnection(b), widths = c(nchar(b) - 1, 1),
                  colClasses = "character")[1] # Number
    
    C <- read.fwf(textConnection(b), widths = c(nchar(b) - 1, 1),
                  colClasses = "character")[2] # Letter for Market Cap
    
    B <- as.numeric(gsub(",", ".", B)) # Substitute comma with dot
    
    if (C == "M"){ B <- B / 1000 } else if (C == "T"){ B <- B * 1000 } 
    
    l <- rbind(l, B) } # Join
    
  colnames(l) <- "Рыночн. кап. Млрд."
  rownames(l) <- x # Stock names
  
  l # Display
}
marketcap.investing.com(c("lukoil_rts", "novatek_rts")) # Test
