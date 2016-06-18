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
#' 
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
        
        
        return(list(
                myInput = c(paste(words[length(words)-2], words[length(words)-1], words[length(words)], sep = "_"),
                            paste(words[length(words)-1], words[length(words)], sep = "_"),
                           words[length(words)]
                            ),
                myTriGram = spooky.32(paste(words[length(words)-2], words[length(words)-1], words[length(words)], sep = "_")),
                myBiGram = spooky.32(paste(words[length(words)-1], words[length(words)], sep = "_")),
                myUniGram = spooky.32(words[length(words)]),
                myToken = spooky.32(words[length(words)])
        ))
}

countMyToken <- function(myGram, myModel, type){
        
        if (type > 1) {
                myGram <- unlist(strsplit(myGram, "_"))
                wd <- myGram[length(myGram)]
                idx <- spooky.32(paste(myGram[1:(length(myGram)-1)], collapse = "_"))
                myModel[(token == wd & index == idx & type == type), freq]
        } else {
                idx <- spooky.32(myGram)
                myModel[(index == idx & type == type), freq]
        }
                
}
