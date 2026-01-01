library("rvest") # Library

names.sa <- function(x){ # Company names from Stock Analysis Website
  
  L <- NULL # Variable for tickers
  
  for (n in 1:length(x)){ a <- x[n] # Tickers
  
    s <- read_html(sprintf("https://stockanalysis.com/stocks/%s/", a)) %>%
      html_nodes('main') %>% .[[1]] %>% html_nodes('div') %>%
      html_nodes('h1') %>% html_text() # Get values

    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        a, which(x == a), length(x)
      )
    )
                         
    s <- read.fwf(textConnection(s), widths = c(nchar(s) - nchar(a) - 3, 1),
                  colClasses = "character")[1] # Reduce excessive elements
    
    L <- rbind.data.frame(L, s) } # Join names
  
  colnames(L) <- "Company Name" # Assign column name
  
  L # Display
}
names.sa(c("AMZN", "ZIM")) # Test
