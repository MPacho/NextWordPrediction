
### Split raw corpus into chunks, clean each of them and save each chunk into file

library(readr)
source("00_process_text_functions.R")
fullCorpus <- read_lines("final/fullCorpus.txt")

### Function that subsets a character vector x into n chunks of size p*original size
### and saves each chunk into a file
splitCorpus <- function(x, n, p) {
  require(readr)
  
  nDocs <- length(x)
  chunkSize <- nDocs*p
  
  i_fullCorpus <- x
  i_nDocs <- nDocs
  
  for ( i in 1:n) {
    set.seed(1)
    i_index <- sample.int(i_nDocs, chunkSize, replace=FALSE)
    i_corpus <- i_fullCorpus[i_index]
    i_cleanCorpus <- completeClean(i_corpus)
    write_lines(i_cleanCorpus, paste0("final/cleanCorpusPart",i,".txt"), append = FALSE)
    
    i_nDocs <- i_nDocs - chunkSize
    i_fullCorpus <- i_fullCorpus[-i_index]
  }
}

splitCorpus(fullCorpus, n=4, p=0.2)

