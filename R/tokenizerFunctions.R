## =============================================================================
##
##              Tokenizer and preprocessing functions
##
## =============================================================================

#' Function to substitute english shorten sentences
#' replaceShortEnglish
#'
#' @param t vector of text as chr
#'
#' @return t vecor of text with substitutions
#' @export
#'
#' @examples Replace I'm by I am, replace I've by I have, etc...
replaceShortEnglish <- function(t){
        t <- gsub(pattern = "can't", replacement = " cannot", t)
        t <- gsub(pattern = "'m", replacement = " am", t)
        t <- gsub(pattern = "ain't", replacement = "am not", t)
        t <- gsub(pattern = "'re", replacement = " are", t)
        t <- gsub(pattern = "'ve", replacement = " have", t)
        t <- gsub(pattern = "'d", replacement = " would", t)
        t <- gsub(pattern = "'ll", replacement = " will", t)
        t <- gsub(pattern = "n't", replacement = " not", t)
        t <- gsub(pattern = "what's", replacement = "what is", t)
        t <- gsub(pattern = "won't", replacement = "will not", t)
        return(t)
}



#' unTwitter
#' Function to clean twitter like content, url, RT and hastags
#'
#' @param t 
#'
#' @return
#' @export
#'
#' @examples
unTwitter <- function(t){
        ## Remove URL with HTTP
        t <- gsub('http\\S+\\s*', '', t)
        ## Remove URL without HTTP
        t <- gsub("\\S+\\.\\S+", "", t)
        ## Remove hastags
        t <- gsub('#\\S+\\s*', '', t)
        # Remove "rt"
        t <- gsub('\\brt\\b', '', t)
        # Remove "rt"
        t <- gsub('\\bu\\b', ' you ', t)
        return(t)
}



#' cleanText
#' Function to remove all characters except english alphabetic and quotes, hypens
#'
#' @param t a vector of text as chr
#'
#' @return t a cleaned vector of text as chr
#' @export
#'
#' @examples
cleanText <- function(t) {
        ## replace non standard quotes
        t <- gsub(pattern = "’", replacement = "'", x = t)
        ## Remove all non alphabetic quote and hyphen, begining space and final space
        #t <- gsub("[^a-z \\-\\']|^ +| +$", "", t, perl = TRUE)
        ## Remove all non single spaces
        #t <- gsub(" {2,}", " ", t, perl = TRUE)
        return(t)
}



#' myTokenizer
#' Our tokenizer function
#'
#' @param v 
#' @param tokenizer 
#'
#' @return v a tokenized list
#' @export
#'
#' @examples
myTokenizer <- function(v, tokenizer = word_tokenizer) {
        
        v %>%
                unTwitter %>%
                cleanText %>%
                replaceShortEnglish %>%
                tokenizer 
        # poerter stemmer
        #  %>% lapply(wordStem, 'en')
}



pickMyGram <- function(myText, stpW) {
        myNgram <- tolower(myText)
        myNgram <- removeNumbers(myNgram)
        myNgram <- unTwitter(myNgram)
        myNgram <- cleanText(myNgram)
        myNgram <- replaceShortEnglish(myNgram)
        myNgram <- removePunctuation(myNgram)
        myNgram <- removeWords(x = myNgram, words = stpW)
        myNgram <- stripWhitespace(myNgram)
        myNgram <- gsub("^ +| +$", "", myNgram, perl = TRUE)
        
        words <- unlist(strsplit(myNgram, " "))
        return(list(
                myTriGram = paste(words[length(words)-2],"_",words[length(words)-1],"_",words[length(words)], sep=""),
                myBiGram = paste(words[length(words)-1],"_",words[length(words)], sep=""),
                myUniGram = words[length(words)])
               )
}



pickMyGramHash <- function(myText, stpW) {
        myNgram <- tolower(myText)
        myNgram <- removeNumbers(myNgram)
        myNgram <- unTwitter(myNgram)
        myNgram <- cleanText(myNgram)
        myNgram <- replaceShortEnglish(myNgram)
        myNgram <- removePunctuation(myNgram)
        myNgram <- removeWords(x = myNgram, words = stpW)
        myNgram <- stripWhitespace(myNgram)
        myNgram <- gsub("^ +| +$", "", myNgram, perl = TRUE)
        
        words <- unlist(strsplit(myNgram, " "))
        
        ## Extract last 4,3,2,1 grams of the input text
        
        countToken <- length(words)
        
        ## If we have at least 4 tokens
        if ( countToken > 3) {
                input4g <- paste(
                        words[length(words)-3], words[length(words)-2], words[length(words)-1], words[length(words)], sep = "_"
                )
                input3g <- paste(
                        words[length(words)-2], words[length(words)-1], words[length(words)], sep = "_"
                )
                input2g <- paste(
                        words[length(words)-1], words[length(words)], sep = "_"
                )
                input1g <- words[length(words)]
                myQuadGramH = spooky.32(input4g)
                myTriGramH = spooky.32(input3g)
                myBiGramH = spooky.32(input2g)
                myUniGramH = spooky.32(input1g)
        } 
        ## If we have 3 tokens
        if ( countToken == 3){
                input4g <- NA
                input3g <- paste(
                        words[length(words)-2], words[length(words)-1], words[length(words)], sep = "_"
                )
                input2g <- paste(
                        words[length(words)-1], words[length(words)], sep = "_"
                )
                input1g <- words[length(words)]
                myQuadGramH <- NA
                myTriGramH <- spooky.32(input3g)
                myBiGramH <- spooky.32(input2g)
                myUniGramH <- spooky.32(input1g)
        }
        ## If we have 2 tokens
        if ( countToken == 2){
                input4g <- NA
                input3g <- NA
                input2g <- paste(
                        words[length(words)-1], words[length(words)], sep = "_"
                )
                input1g <- words[length(words)]
                myQuadGramH <- NA
                myTriGramH <- NA
                myBiGramH <- spooky.32(input2g)
                myUniGramH <- spooky.32(input1g)
        }
        ## If we have 1 tokens
        if ( countToken == 1){
                input4g <- NA
                input3g <- NA
                input2g <- NA
                input1g <- words[length(words)] 
                myQuadGramH <- NA
                myTriGramH <- NA
                myBiGramH <- NA
                myUniGramH <- spooky.32(input1g)
                }
        ## If we have 0 tokens
        if ( countToken == 0){
                input4g <- NA
                input3g <- NA
                input2g <- NA
                input1g <- NA 
                myQuadGramH <- NA
                myTriGramH <- NA
                myBiGramH <- NA
                myUniGramH <- NA
        }
        
        
        return(list(
                myWords = words,
                countToken =  countToken,
                myInputGrams = c(input4g,
                                 input3g,
                                 input2g,
                                 input1g),
                myQuadGramH = myQuadGramH,
                myTriGramH = myTriGramH,
                myBiGramH = myBiGramH,
                myUniGramH = myUniGramH
        ))        
        
}

countMyToken <- function(myGram, myNgModel, type){
        if(!is.null(myGram) & !is.na(myGram)){
                if (type > 1) {
                        myGram <- unlist(strsplit(myGram, "_"))
                        wd <- myGram[length(myGram)]
                        idx <- spooky.32(paste(myGram[1:(length(myGram)-1)], collapse = "_"))
                        a <- myNgModel[(token == wd & index == idx & type == type), freq]
                } else {
                        idx <- spooky.32(myGram)
                        message(type)
                        message(idx)
                        a <- myNgModel[(index == idx & type == type), freq]
                        message(nrow(a))
                }} else {
                        a <- 0
                }
        return(a)
}



loadMySample <- function(mySeed, myBlogsFile, myNewsFile, myTwitterFile, mySampleSize){
        # Get number of lines for each files
        nbLines <- as.numeric(
                c(
                        strsplit(system(paste("wc -l ", myBlogsFile), intern = TRUE), split = " ")[[1]][1],
                        strsplit(system(paste("wc -l ", myNewsFile), intern = TRUE), split = " ")[[1]][1],
                        strsplit(system(paste("wc -l ", myTwitterFile), intern = TRUE), split = " ")[[1]][1]
                ))
        names(nbLines) <- c("blogs", "news", "twitter")
        
        # Set seed
        set.seed(mySeed)
        
        ## Blogs data
        con <- file(myBlogsFile, "r")
        # Get full Data
        fullBlogs <- readLines(con, -1, skipNul = TRUE, encoding = 'UTF8')
        # Sample for training set
        nTrain <- sample(length(fullBlogs), round(mySampleSize["blogs"]*length(fullBlogs)))
        blogsSample <- fullBlogs[nTrain]
        # get the remaining
        rem <- fullBlogs[-nTrain]
        remainSample <- sample(rem, round(mySampleSize["blogs"]*length(rem)))
        # Sample for heldout data and testing data
        nHeldout <- sample(length(remainSample), round(0.6*length(remainSample)))
        heldoutBlogs <- remainSample[nHeldout]
        testingBlogs <- remainSample[-nHeldout]
        close(con)
        
        ## News data
        con <- file(myNewsFile, "r")
        # Get full Data
        fullNews <- readLines(con, -1, skipNul = TRUE, encoding = 'UTF8')
        # Sample for training set
        nTrain <- sample(length(fullNews), round(mySampleSize["news"]*length(fullNews)))
        newsSample <- fullNews[nTrain]
        # get the remaining
        rem <- fullNews[-nTrain]
        remainSample <- sample(rem, round(mySampleSize["news"]*length(rem)))
        # Sample for heldout data and testing data
        nHeldout <- sample(length(remainSample), round(0.6*length(remainSample)))
        heldoutNews <- remainSample[nHeldout]
        testingNews <- remainSample[-nHeldout]
        close(con) 
        
        ## Twitter data
        con <- file(myTwitterFile , "r")
        # Get full Data
        fullTwitter <- readLines(con, -1, skipNul = TRUE, encoding = 'UTF8')
        # Sample for training set
        nTrain <- sample(length(fullTwitter), round(mySampleSize["twitter"]*length(fullTwitter)))
        twitterSample <- fullTwitter[nTrain]
        # get the remaining
        rem <- fullNews[-nTrain]
        remainSample <- sample(rem, round(mySampleSize["twitter"]*length(rem)))
        # Sample for heldout data and testing data
        nHeldout <- sample(length(remainSample), round(0.6*length(remainSample)))
        heldoutTwitter <- remainSample[nHeldout]
        testingTwitter <- remainSample[-nHeldout]
        close(con)
        
        return(list(
                blogsSample = blogsSample,
                heldoutBlogs = heldoutBlogs,
                testingBlogs = testingBlogs,
                newsSample = newsSample, 
                heldoutNews = heldoutNews,
                testingNews = testingNews,
                twitterSample = twitterSample,
                heldoutTwitter = heldoutTwitter,
                testingTwitter = testingTwitter
                )
               )
}


makeTextSample <- function(sampleSizeM) {
        # Configuration of the Sample Size
        sampleSize <- c( blogs = 1,
                        news = 1,
                        twitter = 1) * sampleSizeM

        # Files configuration
        blogsFile <- "../../data/final/en_US/en_US.blogs.txt"
        newsFile <- "../../data/final/en_US/en_US.news.txt"
        twitterFile <- "../../data/final/en_US/en_US.twitter.txt"

        textSample <- loadMySample(
                mySeed = 1234,
                myBlogsFile = blogsFile,
                myNewsFile = newsFile,
                myTwitterFile = twitterFile,
                mySampleSize = sampleSize
        )
        
        save(
                textSample,
                file = paste(
                        '../../data/Rda/sampleText',
                        sampleSizeM,
                        '.Rda',
                        sep = "_"
                )
        )
        return(textSample)
}



makeTokens <- function(myText, sampleSizeM) {
        tokens <- myText %>%
                tolower %>%
                removeNumbers %>%
                myTokenizer
        names(tokens) <- c("blogsSample", "newsSample", "twitterSample")
        
        save(tokens, file = paste('../../data/Rda/tokens_', sampleSizeM,'.Rda', sep = ''))
        return(tokens)
}

extract_bigrams <- function(myText, mySw){
        
        tokens <- myText %>%
                tolower %>%
                removeNumbers %>%
                myTokenizer
        
        ## Create the iterator on the tokens
        it <- itoken(tokens, progessbar = FALSE)
        
        ## Generate the vocabulary using the iterator, 2-gram badwords / stopwords list.
        vocab <- create_vocabulary(it, ngram = c(ngram_min = 2L, ngram_max = 2L), stopwords =  mySw)
        vocab$vocab$terms
}

extract_unigrams <- function(myText, mySw){
        
        tokens <- myText %>%
                tolower %>%
                removeNumbers %>%
                myTokenizer
        
        ## Create the iterator on the tokens
        it <- itoken(tokens, progessbar = FALSE)
        
        ## Generate the vocabulary using the iterator, 2-gram badwords / stopwords list.
        vocab <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L), stopwords =  mySw)
        vocab$vocab$terms
}



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
