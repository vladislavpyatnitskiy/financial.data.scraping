library("rvest") # Library

investing.com.betas <- function(x){ # Beta coefficients of Russian stocks
 
  p <- read_html(sprintf("https://ru.investing.com/equities/%s", x))
  
  price.yahoo1 <- p %>% html_nodes('table') %>% .[[1]] -> tab
  
  B <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_nodes('div') %>%
    html_nodes('a') %>% html_attr('href')
  
  l <- NULL
  tickers <- NULL
  
  for (n in 1:length(B)){ i <- B[n]
    
    p1 <- read_html(sprintf("https://ru.investing.com%s", i))
    
    price.yahoo1 <- p1 %>% html_nodes('dl') %>% .[[2]] -> tab
    
    V <- tab %>% html_nodes('div') %>% html_nodes('dd') %>% html_text()
    
    l <- rbind(l, as.numeric(gsub(",", ".", V[6])))
    
    y <- p1 %>% html_nodes('h1') %>% html_text()
    
    if (isTRUE(grepl("DR", y)) || isTRUE(grepl("_p", y))){
    
      y <- read.fwf(textConnection(y), widths = c(nchar(y) - 7, 4),
                    colClasses = "character")[,2] }
    
    else { y <- read.fwf(textConnection(y), widths = c(nchar(y) - 5, 4),
                         colClasses = "character")[,2] }
    
    if (isTRUE(y == " (HY")){ y <- "HYDR" }
    if (isTRUE(y == " (VK")){ y <- "VKCO" }
    
    tickers <- c(tickers, y)
  }
  rownames(l) <- tickers
  colnames(l) <- "Beta"
  
  l
}
investing.com.betas("russia")
