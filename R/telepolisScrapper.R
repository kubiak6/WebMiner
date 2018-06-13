getTelepolisData <- function(urls)
{
  # vars for status
  bufferSIze <- length(urls)
  counter <- 0
  
  scrapComments <- function(url)
  {
    tryCatch(
      {
        
        article.source = url
        
        #print(paste0("Rozpoczynam pobieranie : ",url))
        
        webpage <- read_html(url)
        
        comment.id<- html_nodes(webpage,'.nr') %>% 
          html_text() %>% as.numeric()
        
        # Ommit articles with no comments
        counter <<- counter + 1
        
        # Article's Title
        article.title <- html_nodes(webpage,'h1') %>% 
          html_text()
        
        # Publication Time
        article.time <- html_nodes(webpage,'.ml5') %>% 
          html_text() %>% str_trim()
        article.time <- article.time[1]
        # Do dorobienia parsowanie
        
        # Article's Content
        article.text <- html_nodes(webpage,'.main_tresc_news')
        if (length(article.text) == 0)
        {
          article.text <- html_nodes(webpage,'.main_tresc')
        }
        
        article.text <- article.text %>% 
          html_text() %>%
          # Remove multiple blanks
          str_replace_all('(  )+',"") %>% 
          # Remove following links to related articles
          str_replace_all('Zobacz.*\n',"") %>%
          # Remove information about source of an article
          str_replace_all('Źródło tekstu:.*\n',"") %>%
          # Remove html markups
          str_replace_all('\n|\t|\r',"") %>%
          # Remove nested scripts
          str_replace_all('<!--.*?-->',"") %>% 
          str_trim()
        
        
        # Raw Date time and optional host
        
        comment.meta.raw <- html_nodes(webpage,'#comments .time') %>% 
          html_text()
        
        # Rozbicie informacji daty i hosta
        HostIndex <- str_locate(comment.meta.raw, "host") %>% 
          as.tibble()
        HostIndexIds <- which(!is.na(HostIndex %>% select(1)))
        
        comment.host <- comment.meta.raw %>% 
          str_sub(HostIndex %>% select(2) %>% unlist() + 2) %>% 
          str_trim()
        
        comment.dttm.raw <- comment.meta.raw
        comment.dttm.raw[HostIndexIds] <- str_trim(str_sub(comment.dttm.raw, 1, (HostIndex %>% select(1) %>% unlist() - 2)))[HostIndexIds]
        comment.dttm <- as_datetime(comment.dttm.raw)
        
        comment.score <- html_nodes(webpage,'.box_oceny span') %>% 
          html_text()
        
        # Komenatrze
        comment.text <- html_nodes(webpage,'.kom_info+ p') %>% 
          html_text()
        
        comment.nickname <- html_nodes(webpage,'#comments .bold') %>% 
          html_text()
        
        article <- tibble(
          article_url   = article.source,
          title         = article.title,
          article.time  = article.time,
          article.text  = article.text
        )
        
        comment <- tibble(
          article_url = article.source,
          id          = comment.id,
          host        = comment.host,
          dttm        = comment.dttm,
          score       = comment.score,
          text        = comment.text,
          nickname    = comment.nickname
        )
        
        print(paste0(format(round(counter/bufferSIze,3)*100, width = 6, nsmall = 2),"%. Pobrano: ", format(length(comment.id), width = 4)," komentarzy."))
        return(list(article, comment))
      },
      error = function(cond)
      {
        print(paste0("ERROR: Błąd przy pobieraniu:", url, cond))
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
  print("------------------- Podsumowanie -----------------");
  print("--------------------------------------------------");
  print("");
  print(paste0("Start               : ", start));
  print(paste0("End                 : ", end));
  print(paste0("Pobrano artykułów   : ", nrow(article)));
  print(paste0("Pobrano komentarzy  : ", nrow(comment)));
  print("");
  print("--------------------------------------------------");
  print("");
  return(list(article, comment))
}