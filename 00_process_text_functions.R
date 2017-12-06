
### Functions for cleaning text.

require(stringi)
require(magrittr)

# Takes in a sting and removes various junk text
removeNonWords <- function(x) {
  x <- x %>%
    stri_replace_all_regex("\\S*[0-9]\\S*", " ") %>% #remove words with numbers
    stri_replace_all_regex("#\\S+", " ") %>% #remove hashtags
    stri_replace_all_regex("http\\S+", " ")  %>% #remove links
    stri_replace_all_regex("\\S*@\\S+", " ") %>% #remove e-mail address and twitter names
    stri_replace_all_regex("\\b(rt|via)\\b", " ") %>% #remove retweet tag
    stri_replace_all_regex(":[dp]\\b", " ") # remove emoticons
  return(x)
} 

# Takes in a string and removes unwanted punctuation leaving just "/" as sentence break
cleanPunct <- function(x) {
  x <- x %>%
    # - remove all punctuation except for several special which will be treated separately
    stri_replace_all_regex("[^[:alnum:] .?!]", " ") %>%
    # - remove trailing whitespace
    stri_trim() %>%
    # - remove any number of .?! at the beginning of document
    stri_replace_all_regex("^[.'?!-]+|[.'?!-]+$", "") %>%
    #  - replace multiple periods, one period preceded by two letters, 
    # question marks and explamation marks by " / "
    stri_replace_all_regex("\\.{2,}|[?!]+|(?<=[a-z][a-z])\\.", " / ") %>%
    # finally remove all remaining periods.
    stri_replace_all_regex("\\.", "")
  return(x)
}

# Takes in a string and cleans some common short forms
cleanWords <- function(x) {
  x <- x %>%
    stri_replace_all_regex("\\bthere *s\\b", " there is ") %>%
    stri_replace_all_regex("\\bthat *s\\b", " that is ") %>%
    stri_replace_all_regex("\\bm\\b", " am ") %>%
    stri_replace_all_regex("\\bre\\b", " are ") %>%
    stri_replace_all_regex("\\baren *t\\b", "arent") %>%
    stri_replace_all_regex("\\bs\\b", " ")  %>%# has? is?
    stri_replace_all_regex("\\bisn *t\\b", "isnt") %>%
    stri_replace_all_regex("\\bain *t\\b", "aint") %>%
    stri_replace_all_regex("(?<=do(es)?)n *t\\b", "nt") %>%
    stri_replace_all_regex("\\bll\\b", " will ") %>%
    stri_replace_all_regex("won *t\\b", "wont") %>% 
    stri_replace_all_regex("\\bve\\b", " have ") %>%
    stri_replace_all_regex("(?<=ha[sv]e?)n *t\\b", "nt") %>%
    stri_replace_all_regex("\\bcan *t\\b", " cannot ") %>%
    stri_replace_all_regex("(?<=ould)n *t\\b", "nt") %>%
    stri_replace_all_regex("(?<=did)n *t\\b", "nt") %>%
    stri_replace_all_regex("(?<=w[ae][sr]e?)n *t\\b", "nt") %>%    
    stri_replace_all_regex("\\bd\\b", " ") %>%  #would? had?
    stri_replace_all_regex("\\be *mail\\b", "email")
  return(x)
}

# Takes a string, cleans it and divides into senstences.
completeClean <- function(x) {
  x_clean_sentences <- x %>%
    stri_enc_toascii() %>% 
    stri_trans_tolower() %>%
    removeNonWords() %>%
    cleanPunct() %>%
    cleanWords() %>%
    stri_split_fixed("/") %>% 
    unlist() %>%
    stri_trim()
  return(x_clean_sentences)
}


#### Tokenize string by whitespace
tokenizeInput <- function(x) {
  tokens <- stri_split_regex(x, "\\p{WHITESPACE}+", omit_empty = TRUE)
  return(tokens)
}

## Takes in a string s and returns a vector of tokens of the last sentence.
cleanAndTokenizeInput <- function(x) {
  tokens <- x %>%
    completeClean() %>%
    tail(1) %>%
    tokenizeInput() %>%
    unlist()
  return(tokens)
}



