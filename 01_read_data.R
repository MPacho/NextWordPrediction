## Read full dataset

library(readr)

## Twitter has very messy spelling with plenty of typos, neglecting punctuation etc.
#twitter <- read_lines("final/en_US/en_US.twitter.txt")
blogs <- read_lines("final/en_US/en_US.blogs.txt")
news <- read_lines("final/en_US/en_US.news.txt")

fullCorpus <- c(blogs, news)
write_lines(fullCorpus, "final/fullCorpus.txt")





