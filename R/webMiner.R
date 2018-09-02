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
database.import()

ex <- etl.extract()
tr <- etl.transform(ex)
etl.load(tr)

database.export()



