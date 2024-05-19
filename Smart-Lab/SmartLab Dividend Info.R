library("rvest") # Library

smartlab.dividend <- function(x){ # Dividends Data Frame for certain year
  
  p <- read_html(sprintf("https://smart-lab.ru/dividends/index?year=%s", x))
  
  div <- p %>% html_nodes('table') %>% .[[1]] -> tab # Table
  
  f <- tab %>% html_nodes('tr') # Dividends
  
  L <- NULL # Show only Dividend Payments that have been approved 
  
  for (n in 1:length(f)){ if (isTRUE(f[n] %>% html_attr('class') ==
                                     "dividend_approved")){
      
      q <- f[n] %>% html_nodes('td') %>% html_text()
      
      L <- c(L, q) } } # Join rows of approved dividends
  
  df <- NULL # Variable for Table with Name, Ticker and values
  
  for (n in 0:(length(L) / 11)){ # Data Frame with Dividend Info
    
    P <- as.numeric(gsub(",",  ".", read.fwf(textConnection(L[(5+n*11)]),
                                             widths=c(nchar(L[(5+n*11)])-1,1),
                                             colClasses = "character")[,1]))
    df <- rbind.data.frame(df,
                           cbind(L[(2 + n * 11)], # Ticker
                                 L[(1 + n * 11)], # Name of Company
                                 as.numeric(gsub(",", ".",
                                                 L[(4 + n * 11)])), # Dividend
                                 P, # Dividend Yield
                                 L[(7 + n * 11)], # Buy Before
                                 L[(8 + n * 11)], # Closing Day
                                 L[(9 + n * 11)], # Payment Date
                                 as.numeric(gsub(",", ".",
                                                 L[(10 + n * 11)])))) } # Price
  
  colnames(df) <- c("Тикер", "Название", "Стоимость дивидендов",
                    "Доходность (%)", "Купить до", "День Закрытия Реестра",
                    "Выплата До", "Цена")
  df # Display
}
smartlab.dividend("2022") # Test
