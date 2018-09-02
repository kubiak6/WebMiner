# init empty databases
database.names <<- c("database.url", "database.article", "database.comment")


database.url_structure <<- tibble(
  url = character(0),
  mod_dt = numeric(0),
  refresh_timestamp = numeric(0)
)

database.article_structure <<- tibble(
  url       = character(0),
  title     = character(0),
  time      = character(0),
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


