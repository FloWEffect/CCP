#### Data Science - Capstone Project - Basic Model

## libraries

library(tidyverse)
library(data.table)
library(quanteda)

# options

options(scipen = 999)

## load the files

load("wf_uni.Rdata")
load("wf_bi.Rdata")
load("wf_tri.Rdata")
load("wf_quad.Rdata")

#### now create a function for finding the best matches

nword_predict <- function(winput) {
  
  winput <- tail(tokens(tolower(winput),
                        remove_numbers=TRUE,
                        remove_punct=TRUE,
                        remove_twitter = TRUE,
                        remove_symbols = TRUE,
                        remove_hyphens = TRUE
  )[[1]],3)
  
  trigram <- paste(tail(winput,3),collapse = ' ')
  bigram <- paste(tail(winput,2),collapse = ' ')
  unigram <- tail(winput,1)
  la <- 0.4
  
  quad <- head(setorder(wf_quad[wf_quad$FirstTerms==trigram,c('LastTerm',"p_stu")],-p_stu),5)
  tri <- head(setorder(wf_tri[wf_tri$FirstTerms==bigram,c('LastTerm',"p_stu")],-p_stu),5)
  tri$p_stu <- la * tri$p_stu
  bi <-  head(setorder(wf_bi[wf_bi$FirstTerms==unigram,c('LastTerm',"p_stu")],-p_stu),5)
  bi$p_stu <- la^2 * bi$p_stu
  uni <- head(wf_uni[,c("LastTerm","p_stu")],5)
  uni$p_stu <- la^3 * uni$p_stu
  
  result <- rbindlist(list(quad,tri,bi,uni))
  result <- as.data.table(aggregate(p_stu~LastTerm, data=result, max))
  head(setorder(result, -p_stu),3)
}

# cat("With Stopwords:")
# cat("\n\n")
# print(head(arrange(result, -p_stu),5))
# cat("Without Stopwords:")
# cat("\n\n")
# result2 <- filter(result, !(LastTerm %in% stopwords()))
# print(head(arrange(result2, -p_stu),5))
