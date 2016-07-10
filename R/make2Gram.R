## =============================================================================
##      V. 2-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab2 <- create_vocabulary(it, ngram = c(ngram_min = 2L, ngram_max = 2L), stopwords = badWords)

biGrams <- data.table(token = vocab2$vocab$terms,
                      freq = vocab2$vocab$terms_count,
                      type = 2)
setorder(biGrams, -freq)
save(biGrams, file = paste('../../data/Rda/sw/biGrams_', sampleSizeM,'.Rda', sep = ''))
rm(biGrams,vocab2)