#### Data Science - Capstone Project - File Generation

## libraries

library(data.table)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stringi)
library(SnowballC)
library(wordcloud)
library(openNLP)
library(RWeka)
library(e1071)

## Parameters

set.seed(56315)                           # set random seed
options(scipen = 999)

## download the data

if (file.exists("Coursera-SwiftKey.zip") == F)
{
  download.file(
    "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
    destfile = "Coursera-SwiftKey.zip"
  )
  unzip("Coursera-SwiftKey.zip")
} else
  print("File already exists!")

## read the data

en_blogs_con <- file("./final/en_US/en_US.blogs.txt", "r")
en_blogs <-
  readLines("./final/en_US/en_US.blogs.txt",
            skipNul = T,
            encoding = "UTF-8")
close(en_blogs_con)

en_news_con <- file("./final/en_US/en_US.news.txt", "r")
en_news <-
  readLines(en_news_con,
            skipNul = T,
            warn = F,
            encoding = "UTF-8")
close(en_news_con)

en_twitter_con <- file("./final/en_US/en_US.twitter.txt", "r")
en_twitter  <-
  readLines(en_twitter_con, skipNul = T, encoding = "UTF-8")
close(en_twitter_con)

## Exploratory Analysis

# Word Counts

en_blogs_wc <- sum(stri_count_boundaries(en_blogs))
en_twitter_wc <- sum(stri_count_words(en_twitter))
en_news_wc <- sum(stri_count_words(en_news))

# Line Counts

en_blogs_lc <- length(en_blogs)
en_twitter_lc <- length(en_twitter)
en_news_lc <- length(en_news)

# File Size

en_blogs_fs <- file.size("./final/en_US/en_US.blogs.txt") / 1024 ^ 2
en_twitter_fs <-
  file.size("./final/en_US/en_US.twitter.txt") / 1024 ^ 2
en_news_fs <- file.size("./final/en_US/en_US.news.txt") / 1024 ^ 2


files_en <- data.frame(
  fileName = c("Blogs", "News", "Twitter"),
  fileSize = c(
    round(en_blogs_fs, digits = 2),
    round(en_twitter_fs, digits = 2),
    round(en_news_fs, digits = 2)
  ),
  lineCount = c(en_blogs_lc, en_twitter_lc, en_news_lc),
  wordCount = c(en_blogs_wc, en_twitter_wc, en_news_wc)
)

colnames(files_en) <-
  c("Filename", "Filesize [MB]", "Linecount", "Wordcount")

## Corpus Creation

# create training, validation and test datasets

en_all <- c(en_blogs, en_news, en_twitter)

n = length(en_all)

trainIndex = sample(1:n, size = round(0.8*n), replace=FALSE)
trainval = en_all[trainIndex]
test = en_all[-trainIndex]

n1 = length(trainval)

trainIndex1 = sample(1:n1, size = round(0.8*n1), replace=FALSE)
train = trainval[trainIndex1]
validation = trainval[-trainIndex1]

# write the data

save(train, file = "train.Rdata")
save(test, file = "test.Rdata")
save(validation, file = "validation.Rdata")

### clear the workspace

rm(list=ls())
gc(reset = T)