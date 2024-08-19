lapply(c("rvest", "timeSeries"), require, character.only = T) # Libraries

kaggle.data <- function(x){ # Transform Data from Kaggle
  
  y <- read_html(x) %>% html_nodes('body') %>% html_nodes('p') %>% html_text()
  
  d <- unlist(strsplit(as.character(gsub('["\r"]', '',
                                         strsplit(toString(y),
                                                  "\\n")[[1]])),"\\,"))
  
  D <- data.frame(d[seq(from = 1, to = length(d), by = 6)],
                  d[seq(from = 5, to = length(d), by = 6)])
  
  D <- D[-1,] # Eliminate excessive row
  
  dates <- D[,1] # Subtract dates from main data frame
  
  D <- as.data.frame(as.numeric(D[,-1])) # Make data numeric
  
  rownames(D) <- dates # Assign dates as row names
  
  D <- as.timeSeries(D) # Make data time series
  
  w <- unlist(strsplit(as.character(x), "D1"))[2] # Clean URL for ticker 
  w <- strsplit(toString(strsplit(toString(as.character(w)),
                                  "\\/")[[1]][2]), "\\_")[[1]][1]  
  
  colnames(D) <- c(w) # Assign Column name
  
  D # Display
}
kaggle.data("https://storage.googleapis.com/kagglesdsdata/datasets/4238207/7841468/D1/RENI_D1.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20240818%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20240818T063519Z&X-Goog-Expires=259200&X-Goog-SignedHeaders=host&X-Goog-Signature=8d184103e99733b07304161a95973b80ca5b9538f20e185201852c9e089fa338c73a7ac063f1fa0c889e187405bbc9507ba16124c2e4482137a9000884ee8065c7aadaad0e56ac552699dcfc9bf6759e83d1ac475b403e863601515e7f118f35101d83c276d83d974d65e21c5989fb96831155f8421d731370f674c352778d96619b1ffad67e3f58dc543ed633d6c91d27a1db29b3abe3586d07cd1c6be9cd23fea88344af6e40238edc3c619844f6cb62a0b92cbdb4188006dc300f1bfd75d3f9e2865c0cf30c4e1c55eee9f01cf6c7ae382c5bb95e26c73ad3af38e2fe78a16464ddb72b7634b0e6742315ecb65078af78d4afcd23595b8f32d57f7ed5adc6") # Test
