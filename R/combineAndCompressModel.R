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

library(data.table) # better than data frames
library(text2vec) # need to tokenizer and %>%
library(tm) # need for stopwords and preprocessing
library(hashFunction) # for hashing refrence of n-grams

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#       0. MAIN CONFIGURATION
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Configuration of stop words 
stopWordsAction <- "included" # other choice : removedFull, included, removedShort 

# Filter bellow minFreq
minFreq <- c(
        1, # for unigram
        1, # for bigrams
        1, # for trigrmas
        1 # for quadgrams
)


# Bad words
# Use to filter profanity, used always
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]


## Stop words configuration
## Choose one of the option to match with model creation
## normally should be set up to included to keep all stop words

stopWordsAction <- "included" # other choice : included, removedShort, removedFull
if (stopWordsAction == "removedFull") {
        ## We will keep the stopwords for the model, remove profanity
        myStopWords <- c(badWords,
                         stopwords("en")
        ) 
}
if (stopWordsAction == "removedShort") {
        ## We will keep the stopwords for the model, remove profanity
        myStopWords <- c(badWords,
                         "and", "or"
        ) 
}
if (stopWordsAction == "included") {
        ## We will keep the stopwords for the model, remove profanity
        myStopWords <- c(badWords
                         #,stopwords("en")
        ) 
} 


## -----------------------------------------------------------------------------
##      1. Load the full Model (this takes time AND RAM)
## -----------------------------------------------------------------------------

## Load 1-4 Grams with 0.7 of the corpus, inc stopwords

load('../../data/Rda/sw/uniGrams_0.7.Rda')
#object.size(uniGrams) / 1024 ^2
#51.5396881103516 bytes
load('../../data/Rda/sw/biGrams_0.7.Rda')
#object.size(biGrams) / 1024 ^2
#923.208602905273 bytes
load('../../data/Rda/sw/triGrams_0.7.Rda')
#object.size(triGrams) / 1024 ^2
#3121.19731140137 bytes
load('../../data/Rda/sw/quadGrams_0.7.Rda')
# object.size(quadGrams) / 1024 ^2
# 5094.49355316162 Mb

## -----------------------------------------------------------------------------
##      2. Remove low frequency terms
## -----------------------------------------------------------------------------

#object.size(uniGrams[freq > 1,]) / 1024 ^2
#18.1388092041016 bytes
uniGrams <- uniGrams[freq > minFreq[1]]
#object.size(biGrams[freq > 1,]) / 1024 ^2
#252.785987854004 bytes
biGrams <- biGrams[freq > minFreq[2]]
#object.size(triGrams[freq > 1,]) / 1024 ^2
#456.306106567383 Mb
triGrams <- triGrams[freq > minFreq[3]]
#object.size(quadGrams[freq > 1,]) / 1024 ^2
#351.12841796875 Mb
quadGrams <- quadGrams[freq > minFreq[4]]


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
#object.size(biGrams) / 1024 ^2
#111.047393798828 bytes

## Separate tri-grams, extract last word from the first 2 words
triGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
triGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
#object.size(triGrams) / 1024 ^2
#241.388916015625 bytes

## Separate tri-grams, extract last word from the first 3 words
quadGrams[,index := gsub(pattern = "(.*)_.*$", replacement = "\\1", x = token)]
quadGrams[,token := gsub(pattern = ".*_(.*)$", replacement = "\\1", x = token)]
#object.size(quadGrams) / 1024 ^2
#237.781982421875 bytes

## Save the intemediary models on the disk
save(uniGrams, biGrams, triGrams, quadGrams, file = '../Models/1-4_Grams_with_sw_limit1.Rda')

## Reload the model
load('../Models/1-4_Grams_with_sw_limit1.Rda')


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
                        quadGrams),
                use.names = TRUE,
                fill = TRUE
        )
# object.size(myModel) / 1024 ^2
# 598.253349304199 Mb

## For uni-grams, replace the missing index by the tolen
myModel[ type == 1, index := token]



## Create the hash function using the package hashFunction::
## we use the spooky.32, there are other option not tried yet
myHashFunction <- function(x) as.numeric(spooky.32(x))

## hash the column index, replace it by the hash
myModel[, index := sapply(as.vector(index), myHashFunction)]
# We save 190 Mo of object size
# object.size(myModel) / 1024 ^2
# 408.385925292969 Mb

## we use the log the Freq instead of Freq to compact Freq column
## 
myModel[, freq := as.integer(round(log(freq)*10))]
# We saved antoehr 50 Mo
# object.size(myModel) / 1024 ^2
# 358.8916

save(myModel, file = '../Models/modelCompact_0.7.Rda')
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

load(file = '../Models/modelCompact_0.7.Rda')
