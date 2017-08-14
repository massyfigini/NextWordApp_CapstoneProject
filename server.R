# Massimiliano Figini 2017-06-18
# The Next Word App

library(shiny)
library(dplyr)
load("WordBigram.RData")
load("WordTrigram.RData")
load("WordQuadrigram.RData")

Found <- 'N'
virgola <- ',\n'

shinyServer(function(input, output) {
  
  observeEvent(input$go, {
    a <- tolower(input$Word)
    a <- unlist(strsplit(a, " ", fixed=TRUE))

    # Algorithm
    if(length(a) > 2) {
      # more then 2 words: first in quadrigram, then trigram, then bigram
      c <- paste(a[length(a)-2], a[length(a)-1], a[length(a)])
      Next <- WordQuadrigram %>% filter(Start == c) %>% select(First,Second,Third)
      # search in trigram
      if(nrow(Next) == 0) {
        b <- paste(a[length(a)-1], a[length(a)])
        Next <- WordTrigram %>% filter(Start == b) %>% select(First,Second,Third)
        if(nrow(Next) == 0) {
          # word not found, search in bigram
          z <- a[length(a)]
          Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
          if(nrow(Next) == 0) {
            # word not found in bigram
            Found <- 'N'
          } else {
            # found in bigram
            Found <- 'B'
            B1 <- Next[1]
            B2 <- Next[2]
            B3 <- Next[3]
          }
        } else {
          # found in trigram
          Found <- 'T'
          T1 <- Next[1]
          T2 <- Next[2]
          T3 <- Next[3]
          # search also in bigram
          z <- a[length(a)]
          Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
          B1 <- Next[1]
          B2 <- Next[2]
          B3 <- Next[3]
        }
        # found in quadrigram
      } else {
        Found <- 'Q'
        Q1 <- Next[1]
        Q2 <- Next[2]
        Q3 <- Next[3]
        # search also in bigram
        z <- a[length(a)]
        Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
        B1 <- Next[1]
        B2 <- Next[2]
        B3 <- Next[3]
      }
      
    } else if(length(a) == 2) {
      # if are two, search in trigram first
      b <- paste(a[1], a[2])
      Next <- WordTrigram %>% filter(Start == b) %>% select(First,Second,Third)
      if(nrow(Next) == 0) {
        # word not found, search in bigram
        z <- a[length(a)]
        Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
        if(nrow(Next) == 0) {
          # word not found
          Found <- 'N'
        } else {
          # found in bigram
          Found <- 'B'
          B1 <- Next[1]
          B2 <- Next[2]
          B3 <- Next[3]
        }
      } else {
        # found in trigram
        Found <- 'T'
        T1 <- Next[1]
        T2 <- Next[2]
        T3 <- Next[3]
        # found also in bigram
        z <- a[length(a)]
        Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
        B1 <- Next[1]
        B2 <- Next[2]
        B3 <- Next[3]
      }
      
    } else {
      # if only one go here
      z <- a[1]
      Next <- WordBigram %>% filter(Start == z) %>% select(First,Second,Third)
      if(nrow(Next) == 0) {
        # word not found
        Found <- 'N'
      } else {
        # found
        Found <- 'B'
        B1 <- Next[1]
        B2 <- Next[2]
        B3 <- Next[3]
      }
    }

    
    output$word <- renderPrint({
      if(Found == 'N') {
      HTML("<font size=5 color=red>Next word not found!</font>")
      } else if(Found == 'B' ) {
        print(unname(B1), row.names=FALSE)
      } else if (Found == 'T'){
        print(unname(T1), row.names=FALSE)
      } else if (Found == 'Q'){
        print(unname(Q1), row.names=FALSE)
      }
    })
  
  
    output$others <- renderPrint({
      if(Found == 'N') {
        HTML("<font size=5 color=red>Words not found!</font>")
      } else if(Found == 'B') {
        print(unname(B2), row.names=FALSE)
        cat(",\n")
        print(unname(B3), row.names=FALSE)
      } else if(Found == 'T'){
        if(as.character(T1$First[1]) != as.character(B1$First[1])) {
          print(unname(B1), row.names=FALSE)
          cat(",\n")
        }
        if(as.character(T1$First[1]) != as.character(B2$Second[1])) {
          print(unname(B2), row.names=FALSE)
          cat(",\n")
        }
        if(as.character(T1$First[1]) != as.character(B3$Third[1])) {
          print(unname(B3), row.names=FALSE)
        }
      } else if(Found == 'Q'){
        if(as.character(Q1$First[1]) != as.character(B1$First[1])) {
          print(unname(B1), row.names=FALSE)
          cat(",\n")
        }
        if(as.character(Q1$First[1]) != as.character(B2$Second[1])) {
          print(unname(B2), row.names=FALSE)
          cat(",\n")
        }
        if(as.character(Q1$First[1]) != as.character(B3$Third[1])) {
          print(unname(B3), row.names=FALSE)
        }
      }
    })
    
  })
})
