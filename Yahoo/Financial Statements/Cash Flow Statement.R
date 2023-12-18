cash.flow.yahoo <- function(x){ # Cash Flow Statement from Yahoo
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/cash-flow?p=%s",x,x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  y <- tab11 %>% html_nodes('div') %>% html_nodes('span') %>%
    html_text()
  
  y # Display
  
  l <- NULL
  h <- NULL
  
  for (n in seq(5)) l <- c(l, y[grep("Breakdown", y) + n])
  
  p <- c("Operating Cash Flow", "Investing Cash Flow", "Financing Cash Flow",
         "End Cash Position", "Capital Expenditure", "Issuance of Debt",
         "Repayment of Debt", "Repurchase of Capital Stock", "Free Cash Flow")
  
  for (m in 1:length(p)){ v <- NULL
  
    for (n in seq(5)){ v <- cbind(v, y[grep(p[m], y) + n])
    
      w <- NULL
    
      if (length(v) > 5){ for (n in seq(0,4,1)) w <- c(w, v[1 + 2*n]) 
    
      } else if (length(v) == 5) { w <- v } } 
    
    h <- rbind(h, w) }
    
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas
  
  rownames(h) <- p # Give names
  colnames(h) <- l # Give dates
  h # Display
}
cash.flow.yahoo("X") # Test
