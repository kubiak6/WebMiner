# init empty databases
database.names <<- c("database.url", "database.article", "database.comment")


database.url_structure <<- tibble(
  tag               = character(0),
  url               = character(0),
  title             = character(0),
  mod_dt            = numeric(0),
  refresh_timestamp = numeric(0)
)

database.article_structure <<- tibble(
  url       = character(0),
  title     = character(0),
  author    = character(0),
  time      = numeric(0),
  tags      = character(0),
  text      = character(0)
)

database.comment_structure <<- tibble(
  url       = character(0),
  id        = numeric(0),
  host      = character(0),
  dttm      = numeric(0),
  score     = numeric(0),
  text      = character(0),
  nickname  = character(0)
)


database.dummy <- function()
{
  database.url <<- database.url_structure
  database.article <<- database.article_structure
  database.comment <<- database.comment_structure
}


# function for importing all databases
database.import <- function()
{
  if(file.exists("database/database.url.csv"))
  {
    importFile <- function(file)
    {
      filepath <- paste0("database/",file,".csv")
      if(file.exists(filepath))
        tryCatch(
          {
            suppressMessages(data <- read_delim(filepath, delim=';'))
            assign(file, data, envir = .GlobalEnv)
            print(paste0("load ",nrow(data), " rows from file ", filepath))
          },
          error = function(cond)
            print(paste0("ERROR: Error during import:", filepath,", ", cond))
        )
      else
      {
        print(paste0("File for ", file, " does not exist."))
        assign(file, NULL, envir = .GlobalEnv)
      }
    }
    output <- lapply(database.names, importFile)
  }
  else
  {
    print(paste0("Database does not exist - initialization of new repository."))
    output <- database.dummy()
  }
}

# function for exporting all databases
database.export <- function()
{
  exportFile <- function(file)
  {
      tryCatch(
        {
          filepath <- paste0("database/",file,".csv")
          data <- eval(parse(text=file))
          write_delim(data, filepath, delim=';')
          print(paste0("save ",nrow(data), " rows to file ", file)) 
        },
        error = function(cond)
        {
          print(paste0("ERROR: Error during export. Filename: ", file,", ", cond))
        }
      )
  }
  output <- lapply(database.names, exportFile)
}

database.downloadDictionaries <- function()
{
  morfologic_url <- "https://github.com/morfologik/polimorfologik/releases/download/2.1/polimorfologik-2.1.zip"
  dest <- "dictionary/polomirfologic.zip"
  download.file(morfologic_url, "dictionary/polomirfologic.zip")
  unzip(dest, files="polimorfologik-2.1.txt", exdir = "dictionary")
  print(paste0(Sys.time(),": DB Controler: downloaded morfologic dictionary from source"))
  
  stopwords_url <- "https://raw.githubusercontent.com/stopwords-iso/stopwords-pl/master/stopwords-pl.txt"
  download.file(stopwords_url, "dictionary/polish_stopwords.txt")
  print(paste0(Sys.time(),": DB Controler: downloaded stopwords dictionary from source"))
  
  sentiment_url <- "http://zil.ipipan.waw.pl/SlownikWydzwieku?action=AttachFile&do=get&target=slownikWydzwieku01.csv"
  download.file(sentiment_url, "dictionary/slownikWydzwieku01.csv")
  print(paste0(Sys.time(),": DB Controler: downloaded sentiment dictionary from source"))
}

# Prepare dictionaries
database.buildDictionaries <- function()
{
  # prepare stopwords list
  print(paste0(Sys.time(),": DB Controler: loading stopwords dictionary..."))
  stopWords <- read_lines("dictionary/polish_stopwords.txt") %>% 
    strsplit(",") %>% 
    unlist() %>% 
    str_trim()
  dict.stop_words_custom <<- c(stopWords, "quot", "pomarańczowy", "fioletowy", "zielony", "play", "orange", "plus", "mobile")
  
  # transform morfologic dataset
  print(paste0(Sys.time(),": DB Controler: polimorfologik dictionary..."))
  dict.morfologic.raw <- read_delim("dictionary/polimorfologik-2.1.txt", delim = ';', col_names = FALSE)
  dict.morfologic <- dict.morfologic.raw %>% 
    separate(X3, into = "X3", extra = "drop", sep=":") %>%
    mutate(
      X3 = case_when(
        X3 == "adj" ~ "przymiotnik",
        X3 %in% c("subst", "ger") ~ "rzeczownik",
        X3 == "adv" ~ "przysłówek",
        X3 == "verb" ~ "czasownik",
        TRUE ~ "inny"
      )
    ) %>% filter(X3 != "inny")
  
  # Build hash for leminization
  print(paste0(Sys.time(),": DB Controler: building hash for leminization..."))
  dict.lema <<- new.env(hash = TRUE)  # equivalent to new.env()
  list2env(
    setNames(
      as.list(dict.morfologic$X1),
      dict.morfologic$X2
    ),
    envir = dict.lema
  )
  
  posNames <- setNames(
    as.list(dict.morfologic$X3),
    dict.morfologic$X2)
  
  # Build hash pos tagging
  print(paste0(Sys.time(),": Miner: building hash for part-of-speach tagging..."))
  dict.pos <<- new.env(hash = TRUE)
  list2env(x = posNames, envir = dict.pos)
  
  # sentiment dictionary
  print(paste0(Sys.time(),": Miner: building hash for sentiment..."))
  dict.sentiment.raw <<- read_delim("dictionary/slownikWydzwieku01.csv", delim = '\t', col_names = FALSE)
  # Build hash for sentiment <-2,-1,0,1,2>
  dict.sent <<- new.env(hash = TRUE)  # equivalent to new.env()
  list2env(
    setNames(
      as.list(dict.sentiment.raw$X4),
      dict.sentiment.raw$X1
    ),
    envir = dict.sent
  )
  dict.sentiment_words <<- dict.sentiment.raw$X1
}
