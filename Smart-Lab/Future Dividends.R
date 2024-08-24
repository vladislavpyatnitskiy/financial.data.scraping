library(rvest) # Library

smartlab.future.divs <- function(x){ # Get data of future dividends
  
  f <- read_html(x) %>% html_nodes('table') %>% .[[1]] %>% html_nodes('tr')
  
  L <- NULL # Show only approved dividends 
  
  for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                     "dividend_approved")){
    
      L <- c(L, f[n] %>% html_nodes('td') %>% html_text()) } }
  
  D <- data.frame(L[seq(from = 2, to = length(L), by = 11)],
                  L[seq(from = 1, to = length(L), by = 11)],
                  format(as.numeric(gsub(",",".",L[seq(from=4, to=length(L),
                                                       by=11)])),scientific=F),
                  as.numeric(gsub(",", ".",
                                  strsplit(L[seq(from=5, to=length(L), by=11)],
                                           split="%"))),
                  L[seq(from = 7, to = length(L), by = 11)],
                  L[seq(from = 8, to = length(L), by = 11)],
                  L[seq(from = 9, to = length(L), by = 11)],
                  format(as.numeric(gsub(",",".",L[seq(from=10, to=length(L),
                                                       by=11)])),scientific=F))
  
  colnames(D) <- c("Тикер","Название","Стоимость дивидендов","Доходность (%)",
                   "Купить до", "День Закрытия Реестра", "Выплата До", "Цена")
  D # Display
}
smartlab.future.divs("https://smart-lab.ru/dividends/") # Test
