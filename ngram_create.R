#### Data Science - Capstone Project - Create the ngram files

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
memory.limit(32000)

## load the files

load("train.Rdata")

# read the profanity filter

profil <- read_table("bad-words.txt", col_names = "term")

#### loop over the data and read all the training data

# split data into chunks

splitc <- split(1:length(train), ceiling(seq_along(1:length(train))/30000))

# set initial empty lists

wf_uni_l <- list()
wf_bi_l <- list()
wf_tri_l <- list()
wf_quad_l <- list()

#### transfer the data incrementally

### Loop 1

for (i in seq(from=1, to=length(splitc))) {
  
  ## create a corpus from the sample data
  
  Corpus_en <- corpus(train[splitc[[i]]])
  
  ## clean the data
  
  tk_uni <- tokens(Corpus_en, remove_numbers = T, remove_punct = T,
                   remove_symbols = T, remove_separators = T,
                   remove_twitter = T, remove_hyphens = T, remove_url = T,
                   ngrams = 1L, concatenator = " ")
  
  # remove profanities
  
  tk_uni <- tokens_remove(tk_uni, profil$term)
  
  # create the ngrams based on the profanity free tokens
  
  tk_bi <- tokens(tk_uni,ngrams = 2, concatenator = " ")
  tk_tri <- tokens(tk_uni,ngrams = 3, concatenator = " ")
  tk_quad <- tokens(tk_uni,ngrams = 4, concatenator = " ")
  
  # Define the Document Term Matrices
  
  dtm <- dfm(tk_uni, tolower = TRUE)
  dtm2 <- dfm(tk_bi, tolower = TRUE)
  dtm3 <- dfm(tk_tri, tolower = TRUE)
  dtm4 <- dfm(tk_quad, tolower = TRUE)
  
  # Create Term Frequency Tables for all created n-grams
  
  wf_uni_l[[i]] <- as.data.table(textstat_frequency(dtm)[,1:2])
  wf_bi_l[[i]] <-  as.data.table(textstat_frequency(dtm2)[,1:2])
  wf_tri_l[[i]] <- as.data.table(textstat_frequency(dtm3)[,1:2])
  wf_quad_l[[i]] <- as.data.table(textstat_frequency(dtm4)[,1:2])
  
  # status
  
  print(i)
  
}

# remove unnecessary objects

rm(list=ls(pattern = "^pro.*|^tk_.*|splitc|train|^dtm.*|Corpus_en"))

# change data types to save some memory

wf_uni_l <- lapply(wf_uni_l , transform, frequency = as.integer(frequency))
wf_bi_l <- lapply(wf_bi_l , transform, frequency = as.integer(frequency))
wf_tri_l <- lapply(wf_tri_l , transform, frequency = as.integer(frequency))
wf_quad_l <- lapply(wf_quad_l , transform, frequency = as.integer(frequency))

# write the lists to file

save(wf_uni_l, file = "wf_uni_l.Rdata")
save(wf_bi_l, file = "wf_bi_l.Rdata")
save(wf_tri_l, file = "wf_tri_l.Rdata")
save(wf_quad_l, file = "wf_quad_l.Rdata")

### clear the workspace

rm(list=ls())
gc(reset = T)


