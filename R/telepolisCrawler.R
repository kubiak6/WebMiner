crawlTelepolisURLs <- function()
{
  webside = "Telepolis"
  
  articlesURLs <- character(0)
  
  url <- "http://www.telepolis.pl/sitemaps/sitemap-news.xml"
  webpage <- read_html(url)
  sitemapXMLs <- str_extract_all(webpage,'http://www.telepolis.pl/.*?.xml')
  
  for (url in unlist(sitemapXMLs))
  {
    webpage <- read_html(url)
    newURLs <- str_extract_all(webpage,'http://www.telepolis.pl/.*?.html')
    newURLs <- unlist(newURLs)
    articlesURLs <- c(articlesURLs, newURLs)
  }
  return(articlesURLs)
}