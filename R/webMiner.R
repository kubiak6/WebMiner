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

# Load scraper sourcecode
source("R/database.R", encoding = "utf-8")

# Load etl engine sourcecode
source("R/etl.R", encoding = "utf-8")

# after import all databases are in the global scope
crawler.setMinArticleDate(ymd("2018-06-01"))

# load persistent db
database.import()

# extract new data
ex <- etl.extract()
tr <- etl.transform(ex)
etl.load(tr)

# save
database.export()

database.url

