library("readr")
# setwd("C:/Users/Mateusz Kubiak/Dropbox/WebMiner")
source("R/telepolisCrawler.R")
source("R/telepolisScrapper.R")


articlesURLs <- crawlTelepolisURLs()
print(paste0("Znaleziono odno?niki do ", length(articlesURLs)," artyków."))

database <- getTelepolisData(articlesURLs[1:12])
articles <- database[[1]]
comments <- database[[2]]

write_delim(articles, path = paste0(getwd(), "/figs/articles.csv"),delim=';')
write_delim(comments, path = paste0(getwd(), "/figs/comments.csv"),delim=';')

View(comments)
View(database)

