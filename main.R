install.packages('rvest')
install.packages("stringr")
install.packages("lubridate")
install.packages("tibble")
install.packages('dplyr')
install.packages("httr")
install.packages("xml2")
install.packages("readr")

library('rvest')
library("stringr")
library("lubridate")
library("tibble")
library('dplyr')
library("httr")
library("xml2")
library("readr")

source("R/telepolisCrawler.R")
source("R/telepolisScrapper.R")


articlesURLs <- crawlTelepolisURLs()
print(paste0("Znaleziono odno?niki do ", length(articlesURLs)," artyków."))

database <- getTelepolisData(articlesURLs[1:20])
articles <- database[[1]]
comments <- database[[2]]

write_delim(articles, path = paste0(getwd(), "/temp/articles.csv"),delim=';')
write_delim(comments, path = paste0(getwd(), "/temp/comments.csv"),delim=';')

View(comments)
View(database)

