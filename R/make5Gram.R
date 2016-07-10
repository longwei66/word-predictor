## =============================================================================
##      VIII. 5-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab5 <- create_vocabulary(it, ngram = c(ngram_min = 5L, ngram_max = 5L), stopwords = badWords)

quintGrams <- data.table(token = vocab5$vocab$terms,
                         freq = vocab5$vocab$terms_count,
                         type = 5)
setorder(quintGrams, -freq)


## Save Unigramds in R objects
save(quintGrams, file = paste('../../data/Rda/sw/quintGrams_', sampleSizeM,'.Rda', sep = ''))
rm(quintGrams,vocab5)