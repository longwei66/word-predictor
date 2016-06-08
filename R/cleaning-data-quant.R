## =============================================================================
##      Cleaning
##
##      - 0. Extract sentence
##      - 1. Create Corpus
##      - 2. Text Cleaning
##      - 3. Tokenize
## =============================================================================


## -----------------------------------------------------------------------------
##      0. Create Corpus
## -----------------------------------------------------------------------------
enSample <- corpus(
        textfile(c("../../data/final/en_US_sample/en_US.blogs_sample.txt",
                 "../../data/final/en_US_sample/en_US.news_sample.txt",
                 "../../data/final/en_US_sample/en_US.twitter_sample.txt"))
)
summaryEn <- summary(enSample)


## Graphical summary
g <- ggplot(data = summaryEn)
g <- g + geom_point(aes(x = Tokens, y = Types, size = Sentences, col = Text))
g <- g + scale_size_area(max_size = 15)
g

## -----------------------------------------------------------------------------
##      1. Text Cleaning
## -----------------------------------------------------------------------------
texts(enSample)[2]


## -----------------------------------------------------------------------------
##      2. Document Feature Matrix
## -----------------------------------------------------------------------------
## Bad Words filtering
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

makeDfm <- function(myCorpus,nbNgram) {
        myDfm <- dfm(myCorpus, 
                     ngrams = nbNgram, concatenator = " ",
                     what = "word",
                     removeNumbers = TRUE, removePunct = TRUE,
                     removeSymbols = TRUE, removeSeparators = TRUE,
                     removeTwitter = TRUE, removeHyphens = TRUE,
                     removeURL = TRUE,
                     ignoredFeatures = c(stopwords("en"), badWords)
        )
        myDfm
}

makeTopFeaturesDfm <- function(aDfm,maxTopic){
        top <- as.data.frame(topfeatures(aDfm, maxTopic))
        names(top) <- c("freq"); top$feature <- rownames(top)
        rownames(top) <- NULL
        top <- dplyr::select(top, feature, freq)
        top
}

plotFeatures <- function(featureDf) {
        g <- ggplot(data = featureDf)
        g <- g + geom_bar(aes(x = reorder(feature, freq), y = freq), stat = "identity")
        g <- g +  coord_flip()
        g
}

enSample1 <- makeDfm(enSample,1)
top1 <- makeTopFeaturesDfm(aDfm = enSample1,maxTopic = 20)
plotFeatures(top1)

enSample2 <- makeDfm(enSample,2)
top2 <- makeTopFeaturesDfm(aDfm = enSample2,maxTopic = 20)
plotFeatures(top2)

enSample3 <- makeDfm(enSample,3)
top3 <- makeTopFeaturesDfm(aDfm = enSample3,maxTopic = 20)
plotFeatures(top3)




#plot(myDfm)
#plot(myDfm, max.words = 500, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))