######################################################################################################################################################
#### Kneser Ney Smoothing

## reformat the tables for use with a backoff model

wf_bi$FirstTerms <- gsub("\\s*\\w*$","",wf_bi$feature)
wf_bi$LastTerm <- gsub("^.*\\<","",wf_bi$feature)

wf_tri$FirstTerms <- gsub("\\s*\\w*$","",wf_tri$feature)
wf_tri$LastTerm <- gsub("^.*\\<","",wf_tri$feature)

wf_quad$FirstTerms <- gsub("\\s*\\w*$","",wf_quad$feature)
wf_quad$LastTerm <- gsub("^.*\\<","",wf_quad$feature)

## apply modified kneser ney smoothing

mkney_uni <- function(dt1) {

  ## unigram preparation
  ##################################################################################################

  # create a table with all the lower ngram frequencys

  ftab <- as.data.table(table(dt1[dt1$frequency <= 4,]$frequency))

  # create the various disfrequencying values based on modified kneser ney (see Chen & Goodman, 1998)

  Y <- ftab[V1==1,N]/(ftab[V1==1,N] + 2 * ftab[V1==2,N])
  d1 <- 1 - 2 * Y * (ftab[V1==2,N]/ftab[V1==1,N])
  d2 <- 2 - 3 * Y * (ftab[V1==3,N]/ftab[V1==2,N])
  d3 <- 3 - 4 * Y * (ftab[V1==4,N]/ftab[V1==3,N])

  # create an adjusted frequency column

  dt1$D_mkn <- ifelse(dt1$frequency == 1, dt1$frequency - d1,
                      ifelse(dt1$frequency == 2, dt1$frequency-d2, dt1$frequency-d3))

  # calculate the 1st term of the modified kneser ney

  dt1$first_term <- (dt1$frequency-dt1$D_mkn)/sum(dt1$frequency)

  # calculate Ns

  dt1$N1 <- length(dt1$frequency[dt1$frequency==1])
  dt1$N2 <- length(dt1$frequency[dt1$frequency==2])
  dt1$N3 <- length(dt1$frequency[dt1$frequency>2])

  # calculate lambda

  dt1$lambda <- ((d1 * dt1$N1)+(d2 * dt1$N2)+(d3 * dt1$N3))/sum(dt1$frequency)

  # calculate the full 2nd term

  dt1$second_term <- dt1$lambda * (dt1$D_mkn/nrow(dt1))

  # finally calculate pknm

  dt1$p_mkn <- dt1$first_term+dt1$second_term

  return(dt1$p_mkn)
}

#### create all other n-grams ####################################################################
##################################################################################################

mkney_bi <- function(dt2) {

  # BIGRAMS

  ##################################################################################################

  # add the recurring terms

  dt2$recurTerms <- sub(".+? ","",dt2$feature)

  # create a table with all the lower ngram frequencys

  ftab <- as.data.table(table(dt2[dt2$frequency <= 4,]$frequency))

  # create the various disfrequencying values based on modified kneser ney (see Chen & Goodman, 1998)

  Y <- ftab[V1==1,N]/(ftab[V1==1,N] + 2 * ftab[V1==2,N])
  d1 <- 1 - 2 * Y * (ftab[V1==2,N]/ftab[V1==1,N])
  d2 <- 2 - 3 * Y * (ftab[V1==3,N]/ftab[V1==2,N])
  d3 <- 3 - 4 * Y * (ftab[V1==4,N]/ftab[V1==3,N])

  # create an adjusted frequency column

  dt2$D_mkn <- ifelse(dt2$frequency == 1, dt2$frequency - d1,
                      ifelse(dt2$frequency == 2, dt2$frequency-d2, dt2$frequency-d3))

  # calculate the 1st term of the modified kneser ney

  dt2$first_term <- (dt2$frequency-dt2$D_mkn)/sum(dt2$frequency)

  # calculate Ns per FirstTerm

  dt2$N1 <- ave(dt2$frequency, dt2$FirstTerms, FUN = function(x) length(x[x==1]))
  dt2$N2 <- ave(dt2$frequency, dt2$FirstTerms, FUN = function(x) length(x[x==2]))
  dt2$N3 <- ave(dt2$frequency, dt2$FirstTerms, FUN = function(x) length(x[x>2]))

  # calculate lambda

  dt2$lambda <- ((d1 * dt2$N1)+(d2 * dt2$N2)+(d3 * dt2$N3))/sum(dt2$frequency)

  # add lower term probability to the data

  dt2 <- merge(dt2, wf_uni[,c("feature","p_mkn")], by.x = "recurTerms", by.y = "feature", all.x = T, incomparables = NA)

  # calculate the full 2nd term

  dt2$second_term <- dt2$lambda * dt2$p_mkn

  # finally calculate pknm

  dt2$p_mkn <- dt2$first_term+dt2$second_term

  return(dt2$p_mkn)
}


##################################################################################################
# TRIGRAMS
##################################################################################################

mkney_tri <- function(dt3) {

  # add the recurring terms

  dt3$recurTerms <- sub(".+? ","",dt3$feature)

  # create a table with all the lower ngram frequencys

  ftab <- as.data.table(table(dt3[dt3$frequency <= 4,]$frequency))

  # create the various disfrequencying values based on modified kneser ney (see Chen & Goodman, 1998)

  Y <- ftab[V1==1,N]/(ftab[V1==1,N] + 2 * ftab[V1==2,N])
  d1 <- 1 - 2 * Y * (ftab[V1==2,N]/ftab[V1==1,N])
  d2 <- 2 - 3 * Y * (ftab[V1==3,N]/ftab[V1==2,N])
  d3 <- 3 - 4 * Y * (ftab[V1==4,N]/ftab[V1==3,N])

  # create an adjusted frequency column

  dt3$D_mkn <- ifelse(dt3$frequency == 1, dt3$frequency - d1,
                      ifelse(dt3$frequency == 2, dt3$frequency-d2, dt3$frequency-d3))

  # calculate the 1st term of the modified kneser ney

  dt3$first_term <- (dt3$frequency-dt3$D_mkn)/sum(dt3$frequency)

  # calculate Ns per FirstTerm

  dt3$N1 <- ave(dt3$frequency, dt3$FirstTerms, FUN = function(x) length(x[x==1]))
  dt3$N2 <- ave(dt3$frequency, dt3$FirstTerms, FUN = function(x) length(x[x==2]))
  dt3$N3 <- ave(dt3$frequency, dt3$FirstTerms, FUN = function(x) length(x[x>2]))

  # calculate lambda

  dt3$lambda <- ((d1 * dt3$N1)+(d2 * dt3$N2)+(d3 * dt3$N3))/sum(dt3$frequency)

  # add lower term probability to the data

  dt3 <- merge(dt3, wf_bi[,c("feature","p_mkn")], by.x = "recurTerms", by.y = "feature", all.x = T, incomparables = NA)

  # calculate the full 2nd term

  dt3$second_term <- dt3$lambda * dt3$p_mkn

  # finally calculate pknm

  dt3$p_mkn <- dt3$first_term+dt3$second_term

  return(dt3$p_mkn)
}

##################################################################################################

# QUADGRAMS

##################################################################################################


mkney_quad <- function(dt4) {

  # add the recurring terms

  dt4$recurTerms <- sub(".+? ","",dt4$feature)

  # create a table with all the lower ngram frequencys

  ftab <- as.data.table(table(dt4[dt4$frequency <= 4,]$frequency))

  # create the various disfrequencying values based on modified kneser ney (see Chen & Goodman, 1998)

  Y <- ftab[V1==1,N]/(ftab[V1==1,N] + 2 * ftab[V1==2,N])
  d1 <- 1 - 2 * Y * (ftab[V1==2,N]/ftab[V1==1,N])
  d2 <- 2 - 3 * Y * (ftab[V1==3,N]/ftab[V1==2,N])
  d3 <- 3 - 4 * Y * (ftab[V1==4,N]/ftab[V1==3,N])

  # create an adjusted frequency column

  dt4$D_mkn <- ifelse(dt4$frequency == 1, dt4$frequency - d1,
                      ifelse(dt4$frequency == 2, dt4$frequency-d2, dt4$frequency-d3))

  # calculate the 1st term of the modified kneser ney

  dt4$first_term <- (dt4$frequency-dt4$D_mkn)/sum(dt4$frequency)

  # calculate Ns per FirstTerm

  dt4$N1 <- ave(dt4$frequency, dt4$FirstTerms, FUN = function(x) length(x[x==1]))
  dt4$N2 <- ave(dt4$frequency, dt4$FirstTerms, FUN = function(x) length(x[x==2]))
  dt4$N3 <- ave(dt4$frequency, dt4$FirstTerms, FUN = function(x) length(x[x>2]))

  # calculate lambda

  dt4$lambda <- ((d1 * dt4$N1)+(d2 * dt4$N2)+(d3 * dt4$N3))/sum(dt4$frequency)

  # add lower term probability to the data

  dt4 <- merge(dt4, wf_tri[,c("feature","p_mkn")], by.x = "recurTerms", by.y = "feature", all.x = T, incomparables = NA)

  # calculate the full 2nd term

  dt4$second_term <- dt4$lambda * dt4$p_mkn

  # finally calculate pknm

  dt4$p_mkn <- dt4$first_term+dt4$second_term

  return(dt4$p_mkn)

}

######################################################################################################################################################

##### Create Stupid Backoff functions (early draft) ##########################################

######################################################################################################################################################

stu_bo <- function(dt1,dt2,dt3,dt4) {

  # create unigram frequency

  dt1$p_stu <- dt1$frequency/sum(dt1$frequency)

  # merge to the 2grams

  # dt2 <- merge(dt2,dt1[,c("feature", "p_stu_uni")], by.x="LastTerm", by.y = "feature", all.x=T, incomparables=NA)

  dt2$p_stu <- ave(dt2$frequency,dt2$FirstTerms,FUN = function(x) x/sum(x))

  # merge to the 3grams
  #
  # dt3$recurTerms <- sub(".+? ","",dt3$feature)

  # dt3 <- merge(dt3,dt2[,c("feature", "p_stu_bi")], by.x="recurTerms", by.y = "feature", all.x=T, incomparables=NA)

  dt3$p_stu <- ave(dt3$frequency,dt3$FirstTerms,FUN = function(x) x/sum(x))

  # merge to the 4grams

  # dt4$recurTerms <- sub(".+? ","",dt4$feature)

  # dt4 <- merge(dt4,dt3[,c("feature", "p_stu_tri")], by.x="recurTerms", by.y = "feature", all.x=T, incomparables=NA)

  dt4$p_stu<- ave(dt4$frequency,dt4$FirstTerms,FUN = function(x) x/sum(x))

  # store data

  alldt <- list(dt1,dt2,dt3,dt4)
  return(alldt)

}

######################################################################################################################################################

############### good turing smoothing function ##########################

######################################################################################################################################################

gturing <- function(x) {

  #define upper threshold for discounting, everything above considered stable
  k <- 5

  #make df with frequencies of the counts
  df <- as.data.frame(table(x))

  df$x <- as.numeric(as.character(df$x))

  nk <- df[df$x == (k+1),"Freq"]

  n1 <- df[df$x == 1,"Freq"]

  df$disc <- with(df, ifelse(x > k, x, (((x + 1)*(lead(Freq)/Freq) - (x * ((k+1)*nk/n1)))/(1-(((k+1)*nk/n1))))))

  dc <- arrange(merge(x,df, by="x", all.x=T),-x)

  dc$disc

}
