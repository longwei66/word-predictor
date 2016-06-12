## =============================================================================
##
##              Create DTM for n-grams
##              Taking full texts as inputs
##
## =============================================================================

## -----------------------------------------------------------------------------
##      Main Steps
##      I. Libs and configuration
##      II. Load Samples of data
##      III. Tokenize the sample texts
##      IV. 1-Grams
##      V. 2-Grams
##      VI. 3-Grams
##
## -----------------------------------------------------------------------------



## =============================================================================
##      I. Libs and configuration
## =============================================================================

# Clear objects in memory
rm(list = ls(all=TRUE))

# Garbage collection (memory)
gg <- gc(reset = TRUE)


# Configuration of the Sample Size
sampleSizeM <- 0.05
sampleSize <- c(
        blogs = 1,
        news = 1,
        twitter = 1) * sampleSizeM


# Function to get remaining RAM
getRam <- function() {
        r <-
                as.numeric(gsub(
                        pattern = "(.*): +(.*) +(.*)$",
                        replacement = "\\3",
                        x = system("free", intern = TRUE)[3]
                )) / 1024
        round(r)
}

# Load general libraries
library(data.table) # Faster and less memory than dataframes
library(R.utils)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# For Parallel Computing
library("parallel")
library("foreach")
library("doParallel")

# Text mining libraries
library(stringi); library(stringr); library(quanteda); 
library(tm) # Used for preprocessing
library(qdap)
library(SnowballC)
library(text2vec) # The main one we use, super fast

# Source R sripts and functions
source('./tokenizerFunctions.R')

# Set a tracker data frame to record operation time
tracker <- data.frame(operation = "init", time = Sys.time(), free.ram.Mo = getRam())


# Multi core configuration
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl, cores = detectCores() - 1)

# Files configuration
blogsFile <-"../../data/final/en_US/en_US.blogs.txt"
newsFile <- "../../data/final/en_US/en_US.news.txt"
twitterFile <- "../../data/final/en_US/en_US.twitter.txt"

# Get number of lines for each files
nbLines <- as.numeric(
        c(
        strsplit(system(paste("wc -l ", blogsFile), intern = TRUE), split = " ")[[1]][1],
        strsplit(system(paste("wc -l ", newsFile), intern = TRUE), split = " ")[[1]][1],
        strsplit(system(paste("wc -l ", twitterFile), intern = TRUE), split = " ")[[1]][1]
))
names(nbLines) <- c("blogs", "news", "twitter")

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]



# Track
gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation = "Load configuration", time = Sys.time(), free.ram.Mo = getRam()))


## =============================================================================
##      II. Load Samples of data
## =============================================================================

# Set seed
set.seed(1234)

## Blogs data
con <- file(blogsFile, "r") 
blogsSample <- sample(readLines(con, -1, skipNul = TRUE), round(sampleSize["blogs"]*nbLines["blogs"]))
close(con)
## News data
con <- file(newsFile, "r")
newsSample <- sample(readLines(con, -1, skipNul = TRUE), round(sampleSize["news"]*nbLines["news"]))
close(con) 
## Twitter data
con <- file(twitterFile , "r")
twitterSample <- sample(readLines(con, -1, skipNul = TRUE), round(sampleSize["twitter"]*nbLines["twitter"]))
close(con)


gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation = paste("Load Data Sample :", sampleSizeM), time = Sys.time(), free.ram.Mo = getRam()))





## =============================================================================
##      III. Tokenize the sample texts
## =============================================================================

tokens <- c(paste(blogsSample, collapse = " "),
            paste(newsSample, collapse = " "),
            paste(twitterSample, collapse = "")
) %>%
        tolower %>%
        removeNumbers %>%
        myTokenizer
names(tokens) <- c("blogsSample", "newsSample", "twitterSample")

# Track
gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation ="Tokenize sample text", time = Sys.time(), free.ram.Mo = getRam()))



## =============================================================================
##      IV. 1-Grams
## =============================================================================

## We will keep the stopwords for the model, remove profanity
myStopWords <- c(badWords
                 #,stopwords("en")
                 ) 

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab1 <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords = myStopWords)

## -----------------------------------------------------------------------------
##      Create DTM of 1-Grams
## -----------------------------------------------------------------------------

## We need to reinitialise iterator : it
it <- itoken(tokens)

## Here we create dtm directly:
v_vectorizer <- vocab_vectorizer(vocab1)
dtm1 <- create_dtm(it, v_vectorizer)
temp <- colSums(dtm1)
## Order and keep only terms appearing more than once
temp <- temp[order(temp, decreasing = TRUE) & temp > 1]
uniGrams <- data.table(token = names(temp),
                       freq = temp,
                       type = 1)
rm(temp,dtm1,vocab1)

## Save Unigramds in R objects
save(uniGrams, file = paste('../../data/Rda/uniGrams_', sampleSizeM,'.Rda', sep = ''))


# Track
gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation ="Generate Unigrams", time = Sys.time(), free.ram.Mo = getRam()))


## =============================================================================
##      V. 2-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab2 <- create_vocabulary(it, ngram = c(ngram_min = 2L, ngram_max = 2L), stopwords = myStopWords)

## -----------------------------------------------------------------------------
##      Create DTM of 2-Grams
## -----------------------------------------------------------------------------

## We need to reinitialise iterator : it
it <- itoken(tokens)

## Here we create dtm directly:
v_vectorizer <- vocab_vectorizer(vocab2)
dtm2 <- create_dtm(it, v_vectorizer)
temp <- colSums(dtm2)
## Order and keep only terms appearing more than once
temp <- temp[order(temp, decreasing = TRUE) & temp > 1]
biGrams <- data.table(token = names(temp),
                       freq = temp,
                       type = 2)
rm(temp,dtm2,vocab2)

## Save Unigramds in R objects
save(biGrams, file = paste('../../data/Rda/biGrams_', sampleSizeM,'.Rda', sep = ''))


# Track
gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation ="Generate Bigrams", time = Sys.time(), free.ram.Mo = getRam()))


## =============================================================================
##      VI. 3-Grams
## =============================================================================

## -----------------------------------------------------------------------------
##      Create Vocabulary
## -----------------------------------------------------------------------------
## Create the iterator on the tokens
it <- itoken(tokens, progessbar = TRUE)

## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
vocab3 <- create_vocabulary(it, ngram = c(ngram_min = 3L, ngram_max = 3L), stopwords = myStopWords)

## -----------------------------------------------------------------------------
##      Create DTM of 3-Grams
## -----------------------------------------------------------------------------

## We need to reinitialise iterator : it
it <- itoken(tokens)

## Here we create dtm directly:
v_vectorizer <- vocab_vectorizer(vocab3)
dtm3 <- create_dtm(it, v_vectorizer)
temp <- colSums(dtm3)
## Order and keep only terms appearing more than once
temp <- temp[order(temp, decreasing = TRUE) & temp > 1]
triGrams <- data.table(token = names(temp),
                      freq = temp,
                      type = 3)
rm(temp,dtm2,vocab2)

## Save Unigramds in R objects
save(triGrams, file = paste('../../data/Rda/triGrams_', sampleSizeM,'.Rda', sep = ''))


# Track
gc(reset = TRUE)
tracker <- rbind(tracker, data.frame(operation ="Generate Unigrams", time = Sys.time(), free.ram.Mo = getRam()))

