## =============================================================================
##
##              Make a basic COMPACT prediction model
##
##              0. Main configuration
##              1. Load the full model from Rda files
##              2. Remove low frequency terms
##              3. create unique data structure by merging models in a data.table
##              4. create hash index, compress freq by log
##
## =============================================================================


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       0. MAIN CONFIGURATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# % of corpus used inthe model
percCorpus <- sampleSizeM

# Filter bellow minFreq
minFreq <- c(
        1, # for unigram
        1, # for bigrams
        1, # for trigrmas
        1, # for quadgrams
        1 # for quintgrams
) * minFreqRatio




## -----------------------------------------------------------------------------
##      1. Load the full Model (this takes time AND RAM)
##      2. Remove low frequency terms
## -----------------------------------------------------------------------------


## Load 1-4 Grams with 0.05 of the corpus, inc stopwords
        
load(paste('../../data/Rda/sw/uniGrams_',percCorpus,'.Rda', sep= ''))
object.size(uniGrams) / 1024 ^2
uniGrams <- uniGrams[freq > minFreq[1]]
object.size(uniGrams)/ 1024 ^2


load(paste('../../data/Rda/sw/biGrams_',percCorpus,'.Rda', sep= ''))
object.size(biGrams) / 1024 ^2
biGrams <- biGrams[freq > minFreq[2]]
object.size(biGrams)/ 1024 ^2



load(paste('../../data/Rda/sw/triGrams_',percCorpus,'.Rda', sep= ''))
object.size(triGrams) / 1024 ^2
triGrams <- triGrams[freq > minFreq[3]]
object.size(triGrams)/ 1024 ^2


load(paste('../../data/Rda/sw/quadGrams_',percCorpus,'.Rda', sep= ''))
object.size(quadGrams) / 1024 ^2
quadGrams <- quadGrams[freq > minFreq[4]]
object.size(quadGrams)/ 1024 ^2



load(paste('../../data/Rda/sw/quintGrams_',percCorpus,'.Rda', sep= ''))
object.size(quintGrams) / 1024 ^2
quintGrams <- quintGrams[freq > minFreq[5]]
object.size(quintGrams)/ 1024 ^2


## -----------------------------------------------------------------------------
##      3. create unique data structure by merging models in a data.table
## -----------------------------------------------------------------------------
## data table structure will be 4 columns
## token : the last word of the n-gram
## freq : the frequency in the corpus
## type : the lenght of the n-gram
## index, the hash of the n-1 gram 

## Separate bigrams, extract last word from the first word
biGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
biGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
object.size(biGrams) / 1024 ^2


## Separate tri-grams, extract last word from the first 2 words
triGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
triGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
object.size(triGrams) / 1024 ^2


## Separate quad-grams, extract last word from the first 3 words
quadGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
quadGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
object.size(quadGrams) / 1024 ^2


## Separate quad-grams, extract last word from the first 3 words
quintGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
quintGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
object.size(quintGrams) / 1024 ^2



## Save the intemediary models on the disk
save(uniGrams, biGrams, triGrams, quadGrams, quintGrams, file = paste('../Models/1-5_Grams_',percCorpus,'_with_sw_limit1.Rda',sep=''))

## Reload the model
load(paste('../Models/1-5_Grams_',percCorpus,'_with_sw_limit1.Rda',sep=''))


## -----------------------------------------------------------------------------
##      4. create hash index, compress freq by log
## -----------------------------------------------------------------------------

## Bind all elementary models in one model
myModel <-
        rbindlist(
                l = list(
                        uniGrams, # not necessary but keep if need data from unigrams
                        biGrams, 
                        triGrams, 
                        quadGrams,
                        quintGrams),
                use.names = TRUE,
                fill = TRUE
        )
object.size(myModel) / 1024 ^2


## For uni-grams, replace the missing index by the tolen
myModel[ type == 1, index := token]



## Create the hash function using the package hashFunction::
## we use the spooky.32, there are other option not tried yet
myHashFunction <- function(x) as.numeric(spooky.32(x))

## hash the column index, replace it by the hash
myModel[, index := sapply(as.vector(index), myHashFunction)]
object.size(myModel) / 1024 ^2


save(myModel, file = paste('../Models/modelCompact_',percCorpus,'.Rda',sep=''))
#          token freq type      index
# 1:         the  150    1 -733979018
# 2:          to  145    1 -733979018
# 3:         and  143    1 -733979018
# 4:           a  143    1 -733979018
# 5:          of  142    1 -733979018
# ---                                 
# 12974634: immediately    7    4 -731928566
# 12974635:    students    7    4  168904541
# 12974636:     tuition    7    4 1842016406
# 12974637:          at    7    4  302397908
# 12974638:      viewed    7    4  475301257








