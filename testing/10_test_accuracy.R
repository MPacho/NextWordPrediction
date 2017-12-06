
library(readr)
library(data.table)
source("04_stupid_backoff_model.R")
#################################################################################
# Prepare data for simple test.

# Takes in a string and splits it randomly
# Outputs a string on which to predict the next word and the true next word
prepare_test_data <- function(s) {
  tokens <- unlist(tokenizeInput(s))
  ntokens <- length(tokens)
  set.seed(123)
  if (ntokens > 1) {
    ix <- sample(seq(2, ntokens), 1)
    x <- paste(tokens[1:(ix-1)], collapse=" ")
    y <- tokens[ix] 
  } else {
    x<-NA; y<-NA
  }
  
  return(list(x,y))
}

testdf <- data.frame(org=read_lines("final/cleanCorpusPart4.txt", n_max=10000))
for (i in 1:nrow(testdf)) {
  temp <- prepare_test_data(testdf[i, "org"])
  testdf[i,"x"] <- temp[[1]]
  testdf[i,"y"] <- temp[[2]]
}

fwrite(testdf[!is.na(testdf[,"x"]),c("x","y")], file="testing/test_data.csv")


############################################################################
# Compute accuracy
testAccuracy <- function(df) {
  correct3 <- c()
  correct1 <- c()
  for (i in 1:nrow(df)) {
    pred <- predictStupidBackoff(df[i, "x"])
    correct3[i] <- if (df[i, "y"] %in% pred) 1 else 0
    correct1[i] <- if (df[i, "y"] == pred[1]) 1 else 0
  }
  
  return(list(top3_accuracy=mean(correct3), top1_accuracy=mean(correct1)))
}
  
testdf <- read_csv("testing/test_data.csv")
testAccuracy(testdf)

#> testAccuracy(testdf)
#$top3_accuracy
#[1] 0.293018

#$top1_accuracy
#[1] 0.1808511


