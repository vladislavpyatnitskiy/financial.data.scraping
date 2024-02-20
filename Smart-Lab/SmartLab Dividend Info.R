library("rvest") # Library

smartlab.dividend <- function(x){ # Get dividend info for certain year
  
  p <- read_html(sprintf("https://smart-lab.ru/dividends/index?year=%s", x))
  
  div <- p %>% html_nodes('table') %>% .[[1]] -> tab
  
  f <- tab %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  df <- NULL # Variable for Table with Name, Ticker and values
  
  for (n in 0:(length(f)/11)){ # Format info to Data Frame
    
    df <- rbind(df, cbind(f[(1 + n * 11)], f[(2 + n * 11)], f[(4 + n * 11)],
                          f[(5 + n * 11)], f[(7 + n * 11)], f[(8 + n * 11)],
                          f[(9 + n * 11)], f[(10 + n * 11)]))}
  
  rownames(df) <- df[,2] # Move ticker column to row names
  df <- df[,-2] # Reduce excess ticker column
  
  colnames(df) <- c("Название","Стоимость дивидендов", "%", "Купить до",
                    "День Закрытия Реестра", "Выплата До", "Цена")
  
  df # Display
}
smartlab.dividend("2022") # Test
