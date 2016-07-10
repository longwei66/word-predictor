## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab1 <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = badWords)


uniGrams <- data.table(token = vocab1$vocab$terms,
                       freq = vocab1$vocab$terms_count,
                       type = 1)
setorder(uniGrams, -freq)
save(uniGrams, file = paste('../../data/Rda/sw/uniGrams_', sampleSizeM,'.Rda', sep = ''))
rm(uniGrams,vocab1)