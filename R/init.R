#file for initialization

install.packages('rvest')
install.packages("stringr")
install.packages("lubridate")
install.packages("tibble")
install.packages('dplyr')
install.packages("httr")
install.packages("xml2")
install.packages("readr")
install.packages("tidytext")
install.packages("tm")
install.packages("SnowballC")
install.packages("tokenizers")
install.packages("tidyr")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("hash")

source("R/database.R", encoding = "utf-8")

#Dwonload required dictionaries
database.downloadDictionaries()