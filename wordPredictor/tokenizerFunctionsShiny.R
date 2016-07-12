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
                        r <- myNgModel[(token == wd & index == idx & type == type),]
                        if ( nrow(r) > 0 ){
                                a <- myNgModel[(token == wd & index == idx & type == type), freq]} else {
                                        ## If data is missing due to compression use the replace 1 algo to count freq of n-1 gram
                                        a <- 1
                                }
                        
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
