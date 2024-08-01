library("rvest") # Library

currency.values <- function(x){ # Currency values from investing.com
  
  p <- read_html(sprintf("https://uk.investing.com/currencies/%s", x)) %>%
    html_nodes('table') %>% html_nodes('tbody') %>% html_nodes('tr') %>%
    html_nodes('span') %>% html_text() # Get data
  
  p <- p[!grepl(":", p) & !grepl("%", p) & !duplicated(p)] 
  
  p <- p[-1] # Delete :, %, blanks, duplicates
  
  l <- data.frame(p[grep("[/]", p)], p[grep("[/]", p) + 1]) # Data frame
  
  for (n in 1:nrow(l)){ if (isTRUE(grepl(",", l[n,2]))){
    
    l[n,2] <- as.numeric(gsub(",", "", l[n,2])) } else {
      
      l[n,2] <- as.numeric(l[n,2]) } } # Make data numeric and reduce commas
  
  colnames(l) <- c("Currency", "Value") # Assign Column names
  
  l # Display
}
currency.values("streaming-forex-rates-majors")
