library("rvest") # Library

smartlab.sectors.all <- function() {
  
  # Some sites reject requests without a User-Agent header
  session <- rvest::session(
    "https://smart-lab.ru/forum/sectors/", 
    httr::user_agent("Mozilla/5.0")
    )
  
  # Walk through all h2 (sector headings) and ul (company lists) elements
  # in the order they appear on the page. Every ul is treated as belonging
  # to the most recently seen h2.
  nodes <- session$response %>% read_html() %>% html_elements("h2, ul")
  
  current_sector <- NA_character_
  rows <- list()
  
  for (node in nodes) { tag <- html_name(node)
    
    if (tag == "h2") { current_sector <- html_text(node, trim = TRUE)
      
    } else if (tag == "ul" && !is.na(current_sector)) {
      
      links <- node %>% html_elements("li > a")
      if (length(links) == 0) next
      
      company <- html_text(links, trim = TRUE)
      href <- html_attr(links, "href")
      
      # Keep only real company links (they point to /forum/<TICKER>)
      company <- company[grepl("/forum/", href, fixed = TRUE)]
      
      if (length(company) > 0) {
        rows[[length(rows) + 1]] <- data.frame(
          company = company,
          sector  = current_sector,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  unique(do.call(rbind, rows))
}
smartlab.sectors.all()
