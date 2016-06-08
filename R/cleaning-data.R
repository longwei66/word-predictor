## =============================================================================
##      Cleaning
##
##      - 0. Extract sentence
##      - 1. Create Corpus
##      - 2. Text Cleaning
##      - 3. Tokenize
## =============================================================================

## Overview
head(englishSample$data$twitter,3)
tail(englishSample$data$twitter,3)
head(englishSample$data$news,3)
tail(englishSample$data$news,3)
head(englishSample$data$blogs,3)
tail(englishSample$data$blogs,3)

# Extract sentences
# See : https://stackoverflow.com/questions/18370518/r-opennlp-could-not-find-function-sentdetect
englishSample$data$news <- sent_detect(englishSample$data$news)
englishSample$data$blogs <- sent_detect(englishSample$data$blogs)
englishSample$data$twitter <- sent_detect(englishSample$data$twitter)



## -----------------------------------------------------------------------------
##      0. Create Corpus
## -----------------------------------------------------------------------------
en <-
        VCorpus(VectorSource(
                list(
                        englishSample$data$news,
                        englishSample$data$blogs,
                        englishSample$data$twitter
                )
        ))
## inspect the corpus
inspect(en)
rm(englishSample)

## -----------------------------------------------------------------------------
##      1. Text Cleaning
## -----------------------------------------------------------------------------
## check available transformations
getTransformations()

## Convert to lowercase
en <- tm_map(en, content_transformer(tolower))

## Remove url
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
en <- tm_map(en, content_transformer(removeURL))

## Keep only alpha numerci
keepAlNum <- function(x) str_replace_all(x, "[^[:alnum:]]", " ")
en <- tm_map(en, content_transformer(keepAlNum))

## Remove numbers
en <- tm_map(en, removeNumbers)

## Remove punctuation
en <- tm_map(en, removePunctuation)

## Bad Words filtering
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]
en <- tm_map(en, removeWords, badWords)

## Stem Document
#en <-tm_map(en, stemDocument)

## Remove extra whitespace
en <-tm_map(en, stripWhitespace)

## -----------------------------------------------------------------------------
##      2. Text Cleaning
## -----------------------------------------------------------------------------
#tk <- MC_tokenizer(en[[1]])
#tk <- scan_tokenizer(en[[1]])
#tk <- tokenizers::tokenize_words(englishSample$data$blogs)
tk <- unlist(tokenizers::tokenize_words(en[[2]]$content))
#tk <- unlist(tokenizers::tokenize_sentences(en[[1]]$content))