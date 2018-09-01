crawler.minArticleDate = ymd("2018-08-01")

crawler.getTelepolisURLs <- function()
{
  webside = "Telepolis"
  
  # empty extracted URLS
  extractedURLs <- tibble()
  
  url <- "http://www.telepolis.pl/sitemaps/sitemap-news.xml"
  webpage <- read_html(url)
  # extract nedsted sitemaps
  sitemapXMLs <- str_extract_all(webpage,'http://www.telepolis.pl/.*?.xml')
  
  for (url in unlist(sitemapXMLs))
  {
    # Extract HTML document
    webpage <- read_html(url)
    
    # Extract table with URLs
    table <- html_nodes(webpage,'url')
    
    # Transform table to <URL, modify date> structure
    newURLs <- table %>% tibble(
      url = table %>% html_nodes('loc') %>% html_text(),
      mod_dt = ymd(table %>% html_nodes('lastmod') %>% html_text())
    ) %>% 
      select(-1) %>% 
      filter(mod_dt > crawler.minArticleDate)
    
    # bind with others extracted URLs
    extractedURLs <- rbind(extractedURLs, newURLs)
  }
  
  print(paste0("Znaleziono odnośniki do ", nrow(extractedURLs)," artyków."))
  
  return(extractedURLs)
  
}
