balance.sheet.yahoo <- function(x){
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s",x,x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  y <- tab11 %>% html_nodes('div') %>% html_nodes('span') %>%
    html_text()
  
  y # Display
  
  l <- NULL
  h <- NULL
  
  for (n in seq(4)) l <- c(l, y[grep("Breakdown", y) + n])
  
  p <- c("Total Assets", "Total Liabilities Net Minority Interest",
         "Total Equity Gross Minority Interest", "Total Capitalization",
         "Common Stock Equity", "Net Tangible Assets", "Working Capital",
         "Invested Capital", "Tangible Book Value", "Total Debt", "Net Debt",
         "Share Issued", "Ordinary Shares Number")
  
  for (m in 1:length(p)){ v <- NULL
  
  for (n in seq(4)){ v <- cbind(v, y[grep(p[m], y) + n])
  
  w <- NULL
  
  if (length(v) > 4){ for (n in seq(0,3,1)) w <- c(w, v[1 + 2*n]) 
  
  } else if (length(v) == 4) { w <- v } } 
  
  h <- rbind(h, w) }
  
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas
  
  rownames(h) <- p # Give names
  colnames(h) <- l # Give dates
  h # Display
}
balance.sheet.yahoo("AAPL") # Test
