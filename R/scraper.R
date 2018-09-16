scraper.getTelepolisData <- function(urls)
{
  # vars for status
  bufferSIze <- length(urls)
  counter <- 0
  
  scrapComments <- function(url)
  {
    tryCatch(
      {
        
        article.source <- url
        
        #print(paste0("Rozpoczynam pobieranie : ",url))
        
        webpage <- read_html(url)
        
        comment.id <- html_nodes(webpage,'.pr-1') %>% 
          html_text() %>% as.numeric()
        if (length(comment.id)==0) comment.id <- NULL
   
        article.tags<- html_nodes(webpage,'.artykul-tagi') %>% 
          html_nodes('a') %>% 
          html_text() %>%
          str_trim() %>% 
          paste(collapse=", ")
        
        counter <<- counter + 1
        
        # Article's Title
        article.title <- html_nodes(webpage,'h1') %>% 
          html_text()
        article.title <- article.title[1]
        
        # Publication Time
        article.metadata<- html_nodes(webpage,'.pr-4') %>% 
          html_text() %>% str_trim()
        article.time <- article.metadata[1] %>% dmy()
        
        article.author <- article.metadata[2] %>% 
          str_extract(":.+") %>% 
          str_sub(3)
        
        # Ommit articles with no comments
        
        # Article's Content
        article.text_raw <- html_nodes(webpage,'.artykul-content')
        
        article.text <- article.text_raw %>% 
          html_text() %>%
          # Remove multiple blanks
          str_replace_all('(  )+',"") %>% 
          # Remove following links to related articles
          str_replace_all('Zobacz.*\n',"") %>%
          # Remove information about source of an article
          str_replace_all('Źródło tekstu:.*\n',"") %>%
          # Remove html markups
          str_replace_all('\n|\t|\r'," ") %>%
          # Remove nested scripts
          str_replace_all('<!--.*?-->'," ") %>%
          # Remove information about tags
          str_replace_all('Tagi:.*',"") %>%
          # Remove duble whitespaces
          str_trim() %>% 
          gsub("\\s+", " ", . ) %>% 
          str_replace("B", "b")
          
        article <- tibble(
          url               = article.source,
          title             = article.title,
          author            = article.author,
          time              = article.time,
          tags              = article.tags,
          text              = article.text
        )
        
        # if there are some comments
        if (!is.null(comment.id))
        {
          # Text
          comment.text <- html_nodes(webpage,'.forum-item .mb-0') %>% 
            html_text() %>% str_trim() %>% 
            gsub("\\s+", " ", . ) %>% 
            str_replace("B", "b") %>% 
            str_replace_all("[^[:graph:]]", " ")
          
          # Rozbicie informacji daty i hosta
          comment.metadata.raw <- html_nodes(webpage,".pt-4 .pr-3") %>% 
            html_text() %>% 
            gsub("\\s+", " ", . ) 
  
          comment.host <- comment.metadata.raw %>% 
            str_match("host: (.*?) ") %>% 
            as.tibble() %>% 
            select(-1) %>% 
            filter(!is.na(V2)) %>% 
            unlist() %>% 
            unname()
          if(length(comment.id) != length(comment.host)) comment.host <- rep("", length(comment.id))
          
          comment.dttm <- comment.metadata.raw %>% 
            str_match("[1-2]{1}[0-9]{3}-[0-9]{2}-[0-9]{2}.+") %>% 
            as.tibble() %>% 
            filter(!is.na(V1)) %>% 
            mutate(date = ymd_hms(V1)) %>% 
            select(2) %>% 
            unlist() %>% 
            as_datetime() %>% 
            unname()
            
          comment.score <- html_nodes(webpage,'b .pr-2') %>% 
            html_text() %>% 
            gsub("\\s+", " ", . ) %>% 
            str_trim() %>% 
            as.numeric()
          
          comment.nickname <- html_nodes(webpage,'.pr-1+ b') %>% 
            html_text() %>% 
            gsub("\\s+", " ", . ) %>% 
            str_trim()
          
          comment <- tibble(
            url         = article.source,
            id          = comment.id,
            host        = comment.host,
            dttm        = comment.dttm,
            score       = comment.score,
            text        = comment.text,
            nickname    = comment.nickname
          )
        } else {
          comment <- database.comment_structure
        }
        
        print(paste0(Sys.time(), ": Scraper: ", format(round(counter/bufferSIze,3)*100, width = 6, nsmall = 2),"%. Scraped: ", format(length(comment.id), width = 4)," comments"))
        return(list(article, comment))
      },
      error = function(cond)
      {
        print(paste0(Sys.time(), ": Scraper ERROR during scraping:", url, ", ", cond))
        return(list(database.article_structure, database.comment_structure))
      }
    )
  }
  start <- Sys.time();
  out <- lapply(urls, scrapComments)
  article <- do.call(rbind, lapply(out, function(out) out[[1]]))
  comment <- do.call(rbind, lapply(out, function(out) out[[2]]))
  end <- Sys.time();
  
  # Stats
  print("");
  print("--------------------------------------------------");
  print("--------------- Scrapping Summary ----------------");
  print("--------------------------------------------------");
  print("");
  print(paste0("Start               : ", start));
  print(paste0("End                 : ", end));
  print(paste0("Scraped articles    : ", nrow(article)));
  print(paste0("Scraped comments    : ", nrow(comment)));
  print("");
  print("--------------------------------------------------");
  print("");
  return(list(article, comment))
}
