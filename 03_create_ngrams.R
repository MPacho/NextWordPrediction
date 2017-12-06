
##############################################################################
library(readr)
library(stringi)
library(data.table)
library(text2vec)
source("00_process_text_functions.R")


###############################################################################
# create partial vocabularies

ngram_min <- 1L
ngram_max <- 4L
nParts_min <- 1L
nParts_max <- 2L

for (part in nParts_min:nParts_max) {
  cleanCorpus <- read_lines(paste0("final/cleanCorpusPart",part,".txt"))

  it <- itoken(cleanCorpus, tokenizer=tokenizeInput)

  for (i in ngram_min:ngram_max) {
    v <- setDT(create_vocabulary(it, ngram=c(i,i), sep_ngram = " "))
    v[, c("term_x", "term_y") := tstrsplit(
                                    stri_replace_last_fixed(term, " ", "_"),
                                    "_", fixed=TRUE)]
    if (i==1) v[, term_x:=""]
    v <- v[, .(term_x, term_y, term_count)]
    
    fwrite(v, file=paste0("ngrams/v", i, "gram_part", part, ".csv"))
  }  

}

###############################################################################
# combine vocabularies, prune them and calculate log probabilities

library(data.table)
library(readr)
library(dplyr)

ngram_min <- 1L
ngram_max <- 4L
nParts_min <- 1L
nParts_max <- 2L
min_count <- 2

for (ngram in ngram_min:ngram_max) {
  pv <- data.table() 
  for (part in nParts_min:nParts_max) {
    pv <- bind_rows(pv, read_csv(paste0("ngrams/v", ngram, "gram_part", part, ".csv")))
    }

  pv <- pv %>% group_by(term_x, term_y) %>%
               summarize(term_count=sum(term_count)) %>%
               filter(term_count>=min_count) %>%
               setDT()
  
  # calculate p
  pv[, "p":= log(term_count) - log(sum(term_count)), by=term_x]
  fwrite(pv[, .(term_x, term_y, p)], file=paste0("ngrams/pv", ngram, "gram.csv"))
}



################################################################################
# Combine all ngram vocabularies into one list

library(readr)
library(data.table)

v <- list()
n <- 4

for (i in 1:n) {
  v[[i]] <- data.table(read_csv(paste0("ngrams/pv", i, "gram.csv")))
  setkey(v[[i]], term_x, term_y)
} 

save(v, file="ngrams.RData")
