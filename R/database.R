# init empty databases
database.names <<- c("database.url", "database.article", "database.comment")

# function for importing all databases
database.import <- function()
{
  importFile <- function(file)
  {
    if(file.exists(file))
      tryCatch(
        assign(file, read_delim(paste0("database/",file), delim=';'), envir = .GlobalEnv),
        error <- function(cond)
          print(paste0("ERROR: Error during import:", file, cond))
      )
    else
    {
      print(paste0("File for ", file, " does not exist - creating empty database..."))
      assign(file, tibble(), envir = .GlobalEnv)
    }
  }
  output <- lapply(database.names, importFile)
}

# function for exporting all databases
database.export <- function()
{
  exportFile <- function(file)
  {
      tryCatch(
        write_delim(file, path = paste0("database/",file), delim=';'),
        error <- function(cond)
          print(paste0("ERROR: Error during export. :", file, cond))
      )
  }
  do.call(exportFile, database.names)
}

# merge extracted URLs with persistent database
database.compareURL <- function(allExtracted)
{
  if(nrow(database.url) > 0)
  {
    allExtracted <- extractedURLS
    
    # only new articles
    new <- allExtracted %>% 
      anti_join(database.url, by="url")
    print(paste0("Liczba nowych URL : ", nrow(new)))
    
    # only articles which were modified or are not older that 3 days
    changed <- allExtracted %>% 
      left_join(
        (database.url %>% rename(new_mod_dt = mod_dt)
         ), by="url") %>%
      filter(mod_dt!=new_mod_dt | (mod_dt + days(3) > Sys.Date())) %>% 
      select("url", "mod_dt")
    print(paste0("Liczba odświeżanych URL : ", nrow(changed)))
      
  
    # remove changed from main db
    old <- database.url %>% 
      anti_join(changed, by="url") 
    print(paste0("Liczba niezmienionych URL : ", nrow(old)))
    
    output <- list(new = new, 
                   changed = changed, 
                   old = old)
    
  }
  else
    output <- list(new = allExtracted, 
                   changed = tibble(), 
                   old = tibble())
  
  return(output)
  
}
