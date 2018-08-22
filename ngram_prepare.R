####### Data Science - Capstone Project - Basic Model

## libraries

library(tidyverse)
library(data.table)

# options

options(scipen = 999)
memory.limit(32000)

### create the ngram models

## unigrams

# load the file

load("wf_uni_l.Rdata")

# create a data table

wf_uni <- setorder(rbindlist(wf_uni_l)[, .(frequency = sum(frequency)), by = feature], -frequency)

# create unigram frequency

wf_uni$p_stu <- wf_uni$frequency/sum(wf_uni$frequency)

# names wf_uni

names(wf_uni)[1] <- "LastTerm"

# prune the data

wf_uni <- wf_uni[wf_uni$frequency > 4,c("LastTerm", "p_stu")]

# save data

save(wf_uni, file = "wf_uni.Rdata")

# clear the workspace

rm(list=ls())
gc(reset = T)

## bigrams

# load the file

load("wf_bi_l.Rdata")

# create a data table

wf_bi <- setorder(rbindlist(wf_bi_l)[, .(frequency = sum(frequency)), by = feature], -frequency)

#reformat the tables for use with a backoff model

wf_bi$FirstTerms <- gsub("\\s*\\w*$","",wf_bi$feature)
wf_bi$LastTerm <- gsub("^.*\\<","",wf_bi$feature)

# create bigram frequency

wf_bi$p_stu <- ave(wf_bi$frequency,wf_bi$FirstTerms,FUN = function(x) x/sum(x))

# prune the data

wf_bi <- wf_bi[wf_bi$frequency > 4,c("FirstTerms","LastTerm","p_stu")]

# save data

save(wf_bi, file = "wf_bi.Rdata")

# clear the workspace

rm(list=ls())
gc(reset = T)

## trigrams

# load the file

load("wf_tri_l.Rdata")

# create a data table

wf_tri <- setorder(rbindlist(wf_tri_l)[, .(frequency = sum(frequency)), by = feature], -frequency)

#reformat the tables for use with a backoff model

wf_tri$FirstTerms <- gsub("\\s*\\w*$","",wf_tri$feature)
wf_tri$LastTerm <- gsub("^.*\\<","",wf_tri$feature)

# create bigram frequency

wf_tri$p_stu <- ave(wf_tri$frequency,wf_tri$FirstTerms,FUN = function(x) x/sum(x))

# prune the data

wf_tri <- wf_tri[wf_tri$frequency > 4,c("FirstTerms","LastTerm","p_stu")]

# save data

save(wf_tri, file = "wf_tri.Rdata")

# clear the workspace

rm(list=ls())
gc(reset = T)

## quadgrams

# load the file

load("wf_quad_l.Rdata")

# create a data table

wf_quad <- setorder(rbindlist(wf_quad_l)[, .(frequency = sum(frequency)), by = feature], -frequency)

#reformat the tables for use with a backoff model

wf_quad$FirstTerms <- gsub("\\s*\\w*$","",wf_quad$feature)
wf_quad$LastTerm <- gsub("^.*\\<","",wf_quad$feature)

# create bigram frequency

wf_quad$p_stu <- ave(wf_quad$frequency,wf_quad$FirstTerms,FUN = function(x) x/sum(x))

# prune the data

wf_quad <- wf_quad[wf_quad$frequency > 4,c("FirstTerms","LastTerm","p_stu")]

# save data

save(wf_quad, file = "wf_quad.Rdata")

# clear the workspace

rm(list=ls())
gc(reset = T)
