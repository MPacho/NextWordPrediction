library(data.table)
library(stringi)
library(dplyr)

source("00_process_text_functions.R")
load("ngrams.RData")


# Predicts next word using Stupid Backoff algorithm
# Takes in a string, number of ngram models to consider n and discount factor lambda
predictStupidBackoff <- function(x, n=4, lambda=0.4, npreds=3) {
  tokens <- cleanAndTokenizeInput(x)
  max_n <- as.integer(min(n, length(tokens)+1))
  log_lambda <- log(lambda)
  sboff <- list() 
  y <- v[[1]][,term_y]

  for (i in 1:max_n) {
    if (i==1) sboff[[i]] <- v[[i]][,.(term_x, term_y, s=p)]
    else {  
      selTokens <- tail(tokens, i-1)   #select tokens for particular ngram model
      term <- paste0(selTokens, collapse=" ")
      
      # prepare shell for stupid backoff scores
      sboff[[i]] <- data.table(expand.grid(term_x=term, term_y=y))
      
      # handy column with i-1 x_term (for joining i-1 scores later on)
      if (i==2) {
        sboff[[i]][, term_x_n1:=term]
        left="term_y"; right="term_y"
      } else {
        sboff[[i]][, term_x_n1:=tstrsplit(stri_replace_first_fixed(term, " ", "_"), "_", fixed=TRUE)[[2]]]
        left=c("term_x_n1", "term_y"); right=c("term_x", "term_y")
      }
      
      sboff[[i]] <- sboff[[i]] %>%
        merge(v[[i]][term], by=c("term_x", "term_y"), all=TRUE) %>% #get p for each (term_x, term_y) pair
        merge(sboff[[i-1]], by.x=left, by.y=right) #get scores s for each (term_x i-1, term_y) pair
      
      # calculate scores
      sboff[[i]][, s:=ifelse(is.na(p), s+log_lambda, p)]
      
      # drop unnecessary columns
      if (i==2) { 
        sboff[[i]] <- sboff[[i]][, .(term_x=term_x.x, term_y, s)]
      } else { sboff[[i]] <- sboff[[i]][, .(term_x, term_y, s)] }                   
    }  
  }

  # find top 3 predictions
  pred <- head(sboff[[max_n]][order(-s), term_y], npreds)
  return(pred)

}




