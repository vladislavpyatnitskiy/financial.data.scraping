library("rvest") # Library

imoex.stocks <- function(x){ # Data Frame of IMOEX Stocks Returns
  
  f <- read_html(x) %>% html_nodes('body') %>% html_nodes('table') %>%
    html_nodes('tr') %>% html_nodes('td') %>% html_text() # Read HTML
  
  l <- NULL # Get data of positive and negative returns
  
  for (n in 0:length(f)){ v <- gsub('["\n"]', '', gsub('["\t"]', '',f[7+17*n]))
    
    v <- as.character(read.fwf(textConnection(v), widths = c(nchar(v) - 1, 1),
                              colClasses = "character")[1])
    
    if (isTRUE(grepl("\\+", v))){ v <- as.numeric(gsub("\\+", "", v)) }
    
    l <- rbind.data.frame(l, cbind(f[2 + 17 * n], as.numeric(v))) }
  
  l <- l[apply(l, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(l) <- c("Компания", "%") # Column names
  
  l # Display
}
imoex.stocks("https://smart-lab.ru/q/index_stocks/IMOEX/") # Test
