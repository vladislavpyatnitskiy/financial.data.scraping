library("rvest") # Library

smartlab.sectors.all <- function(x){ # Data Frames with companies & industries
  
  D <- NULL # variable for data frame
  
  for (n in 1:27){ p <- read_html(sprintf("https://smart-lab.ru/forum/%s/",
                                          x)) %>% html_nodes('body')
  
    d <- data.frame(p %>% html_nodes('ul') %>% .[[n + 1]] %>%
                      html_nodes('li') %>% html_elements('a') %>% html_text(),
                    p %>% html_nodes('h2') %>% .[[n]] %>% html_text() ) # Join
    
    colnames(d) <- c("Компания", "Сектор") # Set up Column Names
    
    D <- rbind(D, d) } # Join data of all industries
    
  D # Display
}
smartlab.sectors.all("sectors") # Test
