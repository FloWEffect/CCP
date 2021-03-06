#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(data.table)
library(quanteda)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ## load the files
  
  load("wf_uni.Rdata")
  load("wf_bi.Rdata")
  load("wf_tri.Rdata")
  load("wf_quad.Rdata")
  
  #### now create a function for finding the best matches
  
  nword_predict <- function(winput, sw=TRUE) {
    
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
    
    quad <- head(setorder(wf_quad[wf_quad$FirstTerms==trigram,c('LastTerm',"p_stu")],-p_stu),10)
    tri <- head(setorder(wf_tri[wf_tri$FirstTerms==bigram,c('LastTerm',"p_stu")],-p_stu),10)
    tri$p_stu <- la * tri$p_stu
    bi <-  head(setorder(wf_bi[wf_bi$FirstTerms==unigram,c('LastTerm',"p_stu")],-p_stu),10)
    bi$p_stu <- la^2 * bi$p_stu
    uni <- head(wf_uni[,c("LastTerm","p_stu")],10)
    uni$p_stu <- la^3 * uni$p_stu
    
    result <- rbindlist(list(quad,tri,bi,uni))
    result$p_stu <- result$p_stu*100
    result <- as.data.table(aggregate(p_stu~LastTerm, data=result, max))
    names(result) <- c("Predicted Word", "Score")
    r1 <- head(setorder(result, -Score),3)
    result2 <- filter(result, !(`Predicted Word` %in% stopwords()))
    r2 <- head(setorder(result2, -Score),3)
    if (sw == TRUE) r1 else r2
  }
  
  # predict the next words
  
  observeEvent(input$predButton,
        output$prediction <- renderTable(isolate(nword_predict(input$predtext)))
  )
  
  observeEvent(input$predButton,
               output$prediction_nosw <- renderTable(isolate(nword_predict(input$predtext, sw = F)))
  )
})
