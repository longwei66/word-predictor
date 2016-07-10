## =============================================================================
##      VI. 3-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab3 <- create_vocabulary(it, ngram = c(ngram_min = 3L, ngram_max = 3L), stopwords = badWords)

triGrams <- data.table(token = vocab3$vocab$terms,
                       freq = vocab3$vocab$terms_count,
                       type = 3)
setorder(triGrams, -freq)

## Save Unigramds in R objects
save(triGrams, file = paste('../../data/Rda/sw/triGrams_', sampleSizeM,'.Rda', sep = ''))
rm(triGrams,vocab3)