## =============================================================================
##      VII. 4-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab4 <- create_vocabulary(it, ngram = c(ngram_min = 4L, ngram_max = 4L), stopwords = badWords)

quadGrams <- data.table(token = vocab4$vocab$terms,
                        freq = vocab4$vocab$terms_count,
                        type = 4)
setorder(quadGrams, -freq)


## Save Unigramds in R objects
save(quadGrams, file = paste('../../data/Rda/sw/quadGrams_', sampleSizeM,'.Rda', sep = ''))
rm(quadGrams,vocab4)