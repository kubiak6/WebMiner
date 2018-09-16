suppressPackageStartupMessages({
library('rvest')
library("stringr")
library("lubridate")
library("tibble")
library('dplyr')
library("httr")
library("xml2")
library("readr")
library("tidytext")
library("tm")
library("SnowballC")
library("tokenizers")
library("tidyr")
library("wordcloud")
library("ggplot2")
library("ggrepel")
library("hash")
library("shiny")
})

# Load scraper sourcecode
source("R/database.R", encoding = "utf-8")

# Load etl sourcecode
source("R/etl.R", encoding = "utf-8")

# Load miner sourcecode
source("R/miner.R", encoding = "utf-8")

# Load reporter sourcecode
source("R/reporter.R", encoding = "utf-8")

# load persistent db
database.import()

# set global parameters
crawler.setMinArticleDate(ymd("2016-01-01"))
crawler.setTagsList(c("play", "plus", "orange", "t-mobile"))

# extract new data
etl.refreshDB()

# update persistant database
database.export()


# text mining dictionaries
# if it is first launch, you need to download external dictionaries
#database.downloadDictionaries()

# build hash structures
database.buildDictionaries()

# output will be the same as minimal crawler date
fromDT <- crawler.minArticleDate


# sentiment

#sentimentAuthorsSay <- miner.textAnalysis("articles", fromDT, tokenType = "sentiment")
#reporter.addOutput("raport_1", "Jakie wyglądają odczucia autorów w czasie?",sentimentAuthorsSay, 60)

#reporter.plot(sentimentAuthorsSay, "sentimentAuthorsSay")

sentimentPeopleSay <- miner.textAnalysis("comments", fromDT, tokenType = "sentiment")
#reporter.plot(sentimentPeopleSay, "sentimentPeopleSay")
#reporter.addOutput("sentimentPeopleSay", "Jak wyglądają odczucia czytelników w czasie?",sentimentPeopleSay, 60)


# unigrams

whatAuthorsSay <- miner.textAnalysis("articles", fromDT, tokenType = "unigram")
#reporter.plot(whatAuthorsSay, "whatAuthorsSay", 100)
reporter.addOutput("whatAuthorsSay", "Jakich słów używają autorzy?",whatAuthorsSay, 100)

#whatPeopleSay <- miner.textAnalysis("comments", fromDT, tokenType = "unigram")
# reporter.addOutput("raport_2", "Jakich słów używają czytelnicy?",whatPeopleSay, 100)
#reporter.plot(whatPeopleSay, "whatPeopleSay", 100)

# bigrams

bigramPeopleSay <- miner.textAnalysis("comments", fromDT, tokenType = "bigram")
#reporter.plot(bigramPeopleSay, "bigramPeopleSay", 60)
reporter.addOutput("bigramPeopleSay", "Jakiech zwrotów używają czytelnicy?",bigramPeopleSay, 60)

#bigramAuthorsSay <- miner.textAnalysis("articles", fromDT, tokenType = "bigram")
#reporter.plot(bigramAuthorsSay, "bigramAuthorsSay", 100)
#reporter.addOutput("raport_2","Jakiech zwrotów używają autorzy?",bigramAuthorsSay, 60)

# feelings

#authorALLFeelings <- miner.textAnalysis("articles", fromDT, tokenType = "unigram", senFlg = TRUE, posFlg = TRUE, senType = "T", posTags = c("przymiotnik", "przysłówek"))
#reporter.addOutput("raport_3","Jakie uczaucia przkazują autorzy?",authorALLFeelings, 100)

peopleALLFeelings <- miner.textAnalysis("comments", fromDT, tokenType = "unigram", senFlg = TRUE, posFlg = TRUE, senType = "T",  posTags = c("przymiotnik", "przysłówek"))
#reporter.plot(peopleALLFeelings, "peopleALLFeelings", 100)
reporter.addOutput("peopleALLFeelings","Co czują czytelnicy?",peopleALLFeelings, 100)


# send to reporter output
reporter.saveOutput()

runApp('report/ShinyWebMinerReport')
