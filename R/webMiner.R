# install.packages('rvest')
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("tibble")
# install.packages('dplyr')
# install.packages("httr")
# install.packages("xml2")
# install.packages("readr")

library('rvest')
library("stringr")
library("lubridate")
library("tibble")
library('dplyr')
library("httr")
library("xml2")
library("readr")

# Load crawler sourcecode
source("R/crawler.R")

# Load scraper sourcecode
source("R/scraper.R")

# Load scraper sourcecode
source("R/database.R")

#  Download full list of aviable articles in Telepolis
database.import()
extract.url.raw <- crawler.getTelepolisURLs()

extract.url.splitList <- database.compareURL(extract.url.raw)
extract.url.new <- extract.splitList$new
extract.url.changed <- extract.splitList$changed
extract.url.unchanged <- extract.splitList$unchanged

extract.url.extractRange <- c(extract.new$url, extract.changed$url)

# Pobranie bazy danych
extract.webside <- scraper.getTelepolisData(extract.extractRange)
extract.article <- extract.webside[[1]]
extract.comment <- extract.webside[[2]]


View(extract.comment)
View(extract.article)


database.export()




