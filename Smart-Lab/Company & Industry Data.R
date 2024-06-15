library("rvest") # Library

smartlab.sectors.all <- function(x){ # Data Frames with companies & industries
  
  D <- NULL # variable for data frame
  
  for (n in 1:27){ p <- read_html(sprintf("https://smart-lab.ru/forum/%s/", x))
    
    l <- p %>% html_nodes('body') %>% html_nodes('ul') %>% .[[n + 1]] -> tab
    h <- p %>% html_nodes('body') %>% html_nodes('h2') %>% .[[n]] -> tab1
    
    h <- tab1 %>% html_text() # Subtract company names and industries
    
    y <- tab %>% html_nodes('li') %>% html_elements('a') %>% html_text()   
    
    d <- data.frame(y, h) # Join
    
    colnames(d) <- c("Компания", "Сектор") # Set up column names
    
    D <- rbind(D, d) } # Join data of all industries
  
  D # Display
}
smartlab.sectors.all("sectors") # Test
