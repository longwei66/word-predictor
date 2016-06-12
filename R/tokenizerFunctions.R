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
        t <- gsub("[^a-z -\\']", "", t)
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
                replaceShortEnglish %>%
                cleanText %>%
                tokenizer 
        # poerter stemmer
        #  %>% lapply(wordStem, 'en')
}
