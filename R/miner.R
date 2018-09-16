miner.posFilter <- function(x, pos)
{ 
  wordTagger <- function(y)
  {
    pos_val <- dict.pos[[y]]
    ret <- ifelse(!is.null(pos_val) && pos_val %in% pos, y, "")
    return(ret)
  }
  out <- paste(sapply(words(x), wordTagger), collapse = ' ')
  return(str_trim(gsub("\\s+", " ", out )))
}


# lemmatization
miner.lemmatization <- function(x)
{
  decode <- function(y)
  {
    new_val <- dict.lema[[y]]
    ret <- ifelse(is.null(new_val), y, new_val)
    return(ret)
  }
  return(paste(sapply(words(x), decode), collapse = ' '))
}

miner.sentimentFilter <- function(x, sign="<>")
{
  wordTagger <- function(y)
  {
    pos_val <- dict.sent[[y]]
    ret <- ifelse(!is.null(pos_val) && 
                    case_when(
                      sign == "<>" ~ pos_val != 0,
                      sign == "+" ~ pos_val > 0,
                      sign == "-" ~ pos_val < 0,
                      sign == "all" ~ TRUE,
                      TRUE ~ TRUE),
                  y, "")
    return(ret)
  }
  out <- paste(sapply(words(x), wordTagger), collapse = ' ')
  return(str_trim(gsub("\\s+", " ", out )))
}


miner.setTextClass <- function(input, className)
{
  if(className %in% c("unigram", "bigram")) 
    class(input) <- append("ngram", class(input))
  
  if(className %in% c("sentiment")) 
    class(input) <- append("unigram", class(input))
  
  class(input) <- append(className, class(input))
  return(input)
}
  
miner.getTextByTag <- function(tagName, contentType, dateFrom, tokenType)
{
  if(contentType=="comments") { 
    content <- database.comment 
  } else { 
    content <- database.article 
    }
  
  # filter by tag
  output <- database.url %>% 
    filter(tag == tagName & dateFrom <= mod_dt) %>%
    anti_join(database.url %>% filter(tag != tagName), by="url") %>% # remove articles with more than one operator
    inner_join(content, by="url") %>%
    select(url, text, mod_dt) %>% 
    rename(doc_id = url)
  
  
  output <- miner.setTextClass(output, tokenType)

  print(paste0(Sys.time(), ": Miner: Extracted from db ", nrow(output), " ", contentType, " as younger than ", dateFrom))
  
  return(output)
}

miner.measures   <- function(x,...) UseMethod("miner.measures")
miner.transform  <- function(x,...) UseMethod("miner.transform")
miner.summarize  <- function(x,...) UseMethod("miner.summarize")

miner.transform.unigram <- function(input, senFlg=FALSE, posFlg=FALSE, senType, posTags)
{
  
  print(paste0(Sys.time(),": Miner: preprocesing..."))
  corpus <- input %>% 
    as.data.frame() %>% 
    DataframeSource() %>% 
    VCorpus()
  
  print(paste0(Sys.time(),": Miner: replacing dots with space..."))
  corpus <- tm_map(corpus,content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))}))
  print(paste0(Sys.time(),": Miner: removing punctuation..."))
  corpus <- tm_map(corpus, removePunctuation)
  print(paste0(Sys.time(),": Miner: removing numbers..."))
  corpus <- tm_map(corpus, removeNumbers)
  print(paste0(Sys.time(),": Miner: transforming to lower..."))
  corpus <- tm_map(corpus, content_transformer(tolower))
  print(paste0(Sys.time(),": Miner: striping whitespaces..."))
  corpus <- tm_map(corpus, stripWhitespace)
  if (posFlg) {
    print(paste0(Sys.time(),": Miner: filtering by pos..."))
    corpus <- tm_map(corpus, content_transformer(miner.posFilter), posTags)
  }
  print(paste0(Sys.time(),": Miner: lemmatization..."))
  corpus <- tm_map(corpus, content_transformer(miner.lemmatization))
  
  if (senFlg) {
    print(paste0(Sys.time(),": Miner: filtering by sentiment..."))
    corpus <- tm_map(corpus, content_transformer(miner.sentimentFilter), senType)
  }
  print(paste0(Sys.time(),": Miner: removing stopwords..."))
  corpus <- tm_map(corpus, removeWords, dict.stop_words_custom)

  print(paste0(Sys.time(),": Miner: converting to tidy..."))
  url <- sapply(corpus, function(x) get("meta",x)$id) %>% unname()
  text <-  sapply(corpus, function(x) get("content",x)) %>% unname()
  mod_dt <- meta(corpus) %>% unlist() %>% unname() %>% as_date()
  
  output <- tibble(
    url = url,
    text = text,
    mod_dt = mod_dt
  ) %>% unnest_tokens(term, text, token="ngrams", n=1)
  
  class(output) <- class(input)
  
  return(output)
}

miner.transform.bigram <- function(input)
{
  
  print(paste0(Sys.time(),": Miner: transforming tidy..."))
  output <- input %>% 
    unnest_tokens(term, text, token = "ngrams", n = 2) %>% 
    separate(term, c("word1", "word2"), sep = " ") %>% 
    mutate(word1 = gsub('[0-9]+', '', word1),
           word2 = gsub('[0-9]+', '', word2)) %>% 
    filter(!(word1 %in% dict.stop_words_custom) & !(word2 %in% dict.stop_words_custom)) %>%
    filter(str_length(word1) > 2 & str_length(word2) > 2) %>%
    unite(term, word1, word2, sep = " ") %>% 
    rename(url = doc_id)
  
  class(output) <- class(input)
  
  return(output)
}

miner.measures.ngram <- function(input)
{
  
  print(paste0(Sys.time(),": Miner: calculating measures..."))
  output <- input %>% 
    filter(term != "") %>% 
    count(url, term, mod_dt) %>% 
    bind_tf_idf(term, url, n) %>% 
    mutate(
      # miara tf * odwrotność idf - czyli zwroty wystepujące w większej liczbie artykułów sa bardziej punktowane
      df = 1/(1+idf), 
      tf_df = tf * df,
      time_wage = 1/log10(as.numeric(Sys.Date() - ymd(mod_dt))),
      time_wage = ifelse(time_wage > 1, 1, time_wage)
      #time_wage = as.numeric(str_sub(year(mod_dt), 4,4))/as.numeric(str_sub(year(Sys.Date()), 4,4))
    ) %>% 
    group_by(term) %>% 
    summarise(
      n = n(),
      doc_wage = mean(df), # zamieniono z tf_df
      time_wage = mean(time_wage)
    )
  class(output) <- class(input)
  
  return(output)
}

miner.measures.sentiment <- function(input)
{
  print(paste0(Sys.time(),": Miner: calculating measures..."))
  output <- input %>% 
    inner_join(dict.sentiment.raw, by=c("term" = "X1")) %>% 
    filter(!is.na(X5)) %>% 
    mutate(mod_dt = floor_date(mod_dt, unit="month"))
  
  class(output) <- class(input)
  return(output)
}

miner.tagToTidy <- function(tagName, contentType, dateFrom, tokenType, ...)
{
  print(paste0(Sys.time(),": Miner: Start proces for: ",tagName))
  comment <- miner.getTextByTag(tagName, contentType, dateFrom, tokenType) %>%
    miner.transform(...) %>% 
    miner.measures() %>% 
    mutate(tag = tagName)
  
  comment <- miner.setTextClass(as.tibble(comment), tokenType)
  return(comment)
}

miner.summarize.ngram <- function(input)
{
  print(paste0(Sys.time(),": Miner: summarizing..."))
  
  output <- input %>% 
    filter(n > 1) %>% 
    bind_tf_idf(term, tag, n) %>% 
    mutate(wage = tf_idf * doc_wage * time_wage)
  
  class(output) <- class(input)
  return(output)
}

miner.summarize.sentiment <- function(input)
{
 
  output <- input %>% 
    group_by(tag, mod_dt) %>% 
    summarise(sentiment = sum(X5)) %>% 
    ungroup()
  
  class(output) <- class(input) 
  return(output)
}

  
miner.textAnalysis <- function(contentType, dateFrom, tokenType, ... )
{
  
  outputList <- lapply(as.list(crawler.tags), miner.tagToTidy, contentType, dateFrom, tokenType, ...)
  output <- do.call(rbind, outputList)

  output <- miner.setTextClass(as.tibble(output), tokenType) %>% 
    miner.summarize()
  
  return(output)
}




