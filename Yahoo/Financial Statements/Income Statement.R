income.statement.yahoo <- function(x){ # Income Statement
  
  p <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", x, x)
  
  page.p <- read_html(p) # Read HTML & extract necessary info
  
  price.yahoo1 <- page.p %>% html_nodes('div') %>% .[[1]] -> tab11
  
  y <- tab11 %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  
  l <- NULL
  h <- NULL
  
  for (n in seq(5)) l <- c(l, y[grep("Breakdown", y) + n])
  
  p <- c("Total Revenue","Cost of Revenue","Gross Profit","Operating Expense",
         "Operating Income", "Other Income Expense", "Pretax Income",
         "Tax Provision", "Net Income Common Stockholders",
         "Diluted NI Available to Com Stockholders","Basic Average Shares",
         "Diluted Average Shares", "Total Expenses",
         "Net Income from Continuing & Discontinued Operation", 
         "Normalized Income", "EBIT", "EBITDA", "Reconciled Cost of Revenue",
         "Reconciled Depreciation",
         "Net Income from Continuing Operation Net Minority Interest",
         "Normalized EBITDA")
  
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
income.statement.yahoo("AAPL") # Test
