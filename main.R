library("readr")

source("lib/telepolisCrawler.R")
source("lib/telepolisScrapper.R")


articlesURLs <- crawlTelepolisURLs()
print(paste0("Znaleziono odno?niki do ", length(articlesURLs)," artyków."))

database <- getTelepolisData(articlesURLs[1:12])
articles <- database[[1]]
comments <- database[[2]]

write_delim(articles, path = paste0(getwd(), "/temp/articles.csv"),delim=';')
write_delim(comments, path = paste0(getwd(), "/temp/comments.csv"),delim=';')

View(comments)
View(database)

