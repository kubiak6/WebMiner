crawler.setMinArticleDate <- function(minDate)
{
  crawler.minArticleDate <<- minDate
}

crawler.setTagsList <- function(tags)
{
  crawler.tags <<- tags
}

# Tag Based Crawler
crawler.getTelepolisTagURLs<- function(tag)
{
  # tag <- "play" 
  
  webside = "Telepolis"
  base <- "https://www.telepolis.pl"
  
  # empty extracted URLS
  extractedURLs <- database.url_structure
  
  # tag index URL
  tagURL <- paste0(base, "/tag/", tag)
  webpage <- read_html(tagURL)
  
  # extract no of indexes for tag
  sitesNo <- suppressWarnings(html_nodes(webpage,'.page-link') %>% html_text() %>% as.numeric() %>% na.omit() %>% max())
  
  print(paste0("Crawler will visit ", sitesNo," indexes to recieve articles URLs about: ", tag))
  
  # for each index
  for (urlNO in seq(sitesNo))
  {

    # get index page
    siteURL <- paste0(tagURL, "?newsPage=", urlNO)
    webpage <- read_html(siteURL)
    
    # parse index informations about articles URLs
    url.nodes <- webpage %>% html_nodes('.col-lg-8') %>% html_nodes('.news-list')
  
    url.info <- tibble(
          tag                 = tag,
          url                 = url.nodes %>% html_nodes('.d-flex') %>% str_extract('a href=".+?"') %>% str_sub(9, str_length(.) - 1),
          title               = url.nodes %>% html_nodes('.header4') %>% html_text(),
          mod_dt              = dmy(url.nodes %>% html_nodes('.pr-3') %>% html_text()),
          refresh_timestamp   = Sys.time() # timestamp of last extraction
    )
    url.info$url <- paste0("https://www.telepolis.pl", url.info$url, "?all=true")
   
    # determine new URLs 
    newURLs <- url.info %>% filter(mod_dt >= crawler.minArticleDate)
    
    # bind new to others extracted URLs
    extractedURLs <- rbind(extractedURLs, newURLs)
    
    # if there are no new articles then break the loop
    if ((url.info %>% filter(mod_dt < crawler.minArticleDate) %>% count() %>% as.numeric()) > 0)
    {
      print(paste0(Sys.time(), ": Crawler for tag: ",tag,", progress: ",format(100, width = 6, nsmall = 2),"%. Found: ", format(nrow(newURLs), width = 2)," URLs, Stop processing due to min crawling date: ", crawler.minArticleDate))
      break;
    }
    else
    {
      # date range
      range <- newURLs %>% 
        select(mod_dt) %>% 
        summarise(minDate = min(mod_dt), maxDate = max(mod_dt)) %>% 
        mutate(range = paste0(as.character(minDate)," <-> ", as.character(maxDate))) %>% 
        select(3) %>% 
        unlist()
      
      print(paste0(Sys.time(), ": Crawler for tag: ",tag,", progress: ",format(round(urlNO/sitesNo,3)*100, width = 6, nsmall = 2),
                   "%. Found: ", format(nrow(newURLs), width = 2)," URLs, range: ",range))
    }
  }
  
  print(paste0(Sys.time(), ": Crawling summary for tag ",tag ,", found: ", nrow(extractedURLs)," URLs."))
  
  return(extractedURLs)
  
}

crawler.getTelepolisURLs <- function()
{
  outputList <- lapply(as.list(crawler.tags), crawler.getTelepolisTagURLs)
  output <- do.call(rbind, outputList)
  return(output)
}