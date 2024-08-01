library("rvest") # Library

commodity.prices <- function(x){ # Commodity Prices from investing.com
  
  p <- read_html(x) %>% html_nodes('table') %>% html_nodes('tbody') %>%
    html_nodes('tr') %>% html_nodes('span') %>% html_text() # Get data
  
  p <- p[-grep("Copper", p)[-c(2,4)]] # Delete excess Copper values
  p[grep("Copper", p)[1]] <- "US Copper" # Assign US 
  p[grep("Copper", p)[2]] <- "UK Copper" # Assign UK
  
  p <- p[!duplicated(p)] # Delete duplicates
  
  p <- p[-c(1,3)] # Delete values
  
  p <- p[!grepl(":", p) & !grepl("%", p)] # Delete values with : and %
  p <- p[-(grep("US Coffee C", p) - 1)] # Delete 
  
  l <- NULL # Make data frame from list
  
  for (n in 0:length(p)){ l <- rbind.data.frame(l, cbind(p[1+2*n], p[2+2*n])) }
  
  l <- l[apply(l, 1, function(x) all(!is.na(x))),] # Delete NA
  
  colnames(l) <- c("Ticker", "Price") # Assign column names
  
  l # Display
}
commodity.prices("https://uk.investing.com/commodities/real-time-futures") 
