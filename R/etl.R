# Load crawler sourcecode
source("R/crawler.R", encoding = "utf-8")

# Load scraper sourcecode
source("R/scraper.R", encoding = "utf-8")

# merge extracted URLs with persistent database
etl.getExtractRange <- function(extractedURLS)
{
  if(!is.null(database.url))
  {
    
    # only new articles
    new <- extractedURLS %>% 
      anti_join(database.url, by="url")
    print(paste0("Liczba nowych URL : ", nrow(new)))
    
    # only articles which were modified or are not older that 3 days
    changed <- extractedURLS %>% 
      inner_join(
        (database.url %>% select(url, mod_dt) %>% rename(new_mod_dt = mod_dt)
         ),by="url") %>%
      filter(mod_dt!=new_mod_dt | (mod_dt + days(3) > Sys.Date())) %>% 
      select(url, mod_dt, refresh_timestamp)
    
    print(paste0("Liczba odświeżanych URL : ", nrow(changed)))
    
    output <- rbind(new, changed)
    
  }
  else
    output <- allExtracted
  
  return(output)
  
}

# function for extracting data
# return only new or changed data
etl.extract <- function()
{
  #  Download full list of aviable articles in Telepolis
  extract.url.raw <- crawler.getTelepolisURLs()
  
  # Compare with current DB
  extract.url <- etl.getExtractRange(extract.url.raw)
  if(nrow(extract.url.raw))
  {
    # scrapp full articles and comments base on extraction range
    extract.webside <- scraper.getTelepolisData(extract.url$url)
    extract.article <- extract.webside[[1]]
    extract.comment <- extract.webside[[2]]
    
    extract <- list(
      url = extract.url,
      article = extract.article,
      comment = extract.comment
    )
  }
  else # nothing to do here...
  {
    extract <- list(
      url     = database.url_structure,
      article = database.article_structure,
      comment = database.comment_structure
    )
  }
  
  return(extract)
}

# function to rebuild database
# return changed instances for all databases
etl.transform <- function(extract)
{
    transform.url <- database.url %>% 
      anti_join(extract$url, by = "url") %>% 
      rbind(extract$url)
    
    # rebuild articles
    transform.article <- database.article %>% 
      anti_join(extract$url, by = "url") %>% 
      rbind(extract$article)
    
    # rebuild comments
    transform.comment <- database.comment %>% 
      anti_join(extract$url, by = "url") %>% 
      rbind(extract$comment)
    
    tranform <- list(
      url     = transform.url,
      article = transform.article,
      comment = transform.comment
    )
  
  return(tranform)
  
}

# function to load database
# return changed instances for all databases
etl.load <- function(transform)
{
  database.url     <<- transform$url
  database.article <<- transform$article
  database.comment <<- transform$comment
}

