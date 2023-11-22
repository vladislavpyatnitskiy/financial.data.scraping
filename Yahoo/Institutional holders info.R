# Function to get info about holders of large positions
s.holders <- function(x){ 
  
  s <- sprintf("https://finance.yahoo.com/quote/%s/holders?p=%s", x, x) # URL
  
  s.page <- read_html(s) # Read HTML of page
  
  s.yahoo <- s.page %>% html_nodes('table') %>% .[[2]] -> tab1 # Assign Table 
  
  s.header <- tab1 %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df.f1 <- NULL # Create lists to contain variables
  df.f2 <- NULL
  
  for (n in 0:(length(s.header) / 5)){ 
    
    df.f1 <- rbind(df.f1, s.header[(1 + n * 5)]) # Ratio names
    
    df.f2 <- rbind(df.f2, s.header[(4 + n * 5)]) } # Ratio values
  
  df.f3 <- data.frame(df.f1, df.f2) # Join 
  
  df.f3 <- df.f3[-nrow(df.f3),] # Subset excess row
  
  df.f3[,2] <- read.fwf(textConnection(df.f3[,2]),
                        widths=c(nchar(df.f3[,2])-1, 1), # Reduce %
                        colClasses = "character")
  
  df.f3[,2] <- as.numeric(df.f3[,2]) # Change format to numeric
  
  df.f3[nrow(df.f3) + 1,]=c("Others", 100-sum(df.f3[,2])) # Numbers for others
  
  df.f3 # Display
}
s.holders("NKE") # Test
