library("rvest") # Library

commodity.prices <- function(x){ # Commodity Prices from investing.com
  
  p <- read_html(x) %>% html_nodes('table') %>% html_nodes('tbody') %>%
    html_nodes('tr') %>% html_nodes('span') %>% html_text() # Get data
  
  p <- p[-grep("Copper", p)[-c(2,4)]] # Delete excess Copper values
  p[grep("Copper", p)[1]] <- "US Copper" # Assign US 
  p[grep("Copper", p)[2]] <- "UK Copper" # Assign UK
  
  p <- p[!duplicated(p)][-c(1,3)] # Delete duplicates & excess values
  
  p[grep("XAU/USD", p)[1]] <- "XAU-USD" # Replace / with - & Delete : % /
  
  p <- p[!grepl(":", p) & !grepl("%", p) & !grepl("/", p)] # Form Data Frame
  
  l <- data.frame(p[seq(from=1,to=length(p),by=2)], p[seq(from=2,to=length(p),
                                                          by=2)])
  colnames(l) <- c("Ticker", "Price") # Assign column names
  
  for (n in 1:nrow(l)){ # Make data numeric and reduce commas
    
    l[n,2]=as.numeric(ifelse(grepl(",", l[n,2])==T,gsub(",","",l[n,2]),l[n,2]))}
  
  l # Display
}
commodity.prices("https://uk.investing.com/commodities/real-time-futures") 
