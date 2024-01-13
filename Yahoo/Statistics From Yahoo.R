library("rvest") # Library

# Function to Scrape Statistics for Security
statistics.yahoo <- function(x, y = 1, transpose = F){ # Security URL
  
  df.s <- NULL # Set up list for infos
  
  for (n in 1:length(x)){ j <- x[n] # For each security get info
  
  s <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",j,j)
  
  s.page <- read_html(s) # Read HTML of page
  
  s.yahoo <- s.page %>% html_nodes('table') %>% .[[y]] -> tab # Assign Table 
  
  s <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df <- NULL # Data Frame values
  
  for (n in 0:(length(s) / 2)){ # Row Values
    
    df <- rbind(df, cbind(s[(1 + n * 2)], s[(2 + n * 2)])) } # Ratio values
    
  df <- df[-nrow(df),] # Reduce excessive row
  
  rownames(df) <- df[,1] # Assign row names
  
  df <- subset(df, select = -c(1)) # Reduce excess column
  
  colnames(df) <- j # Assign column name
  
  if (is.null(df.s)){ df.s <- df } else { df.s <- cbind(df.s, df) } }
  
  if (isTRUE(transpose)){ t(df.s) } else { df.s } # Display
}
# Test
statistics.yahoo(x = c("AAPL", "MSFT", "GOOGL", "AMZN", "META"),1, transpose=F)
