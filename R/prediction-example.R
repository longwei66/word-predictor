## =============================================================================
##
##                      Prediction Example
##
##                      (run from main dir ./)
##
## =============================================================================


## =============================================================================
## Basic configuration
## =============================================================================
## Defin the sampling % of the initial data
## This will configure automatically the model to be used
sampleSizeM <- 0.75 # Available models are 0.75, 0.5, 0.15, 0.05 and 0.03, 0.01
## Make a persistent record of this variable in case we have to restart R Session
save(sampleSizeM, file = './R/sampleSizeM.Rda')

# Main libs
# ----------
source('./R/loadMainLibraries.R', chdir = TRUE)
# Source R sripts and functions
source('./R/tokenizerFunctions.R', chdir = TRUE)
source('./R/predictFunction.R', chdir = TRUE)
load('./R/sampleSizeM.Rda')

# Bad words
# ---------
badWordsUrl <- "./../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]


## =============================================================================
##      Load Final Model
## =============================================================================
#load(file = paste('./Models/modelCompact_',sampleSizeM,'.Rda',sep=''))
load(file = paste('./Models/modelShinyCompact_',sampleSizeM,'_0.4.Rda',sep=''))



## =============================================================================
##      Load the test data
## =============================================================================

# Load Sample text
load(paste('../data/Rda/sampleText_', sampleSizeM, '_.Rda', sep = ""))

# Set the seed
set.seed(29706)

# Build a corpus of 100 lines of the initial test data
TestCorpus <- quanteda::corpus(base::sample(c(textSample$testingBlogs #,
                                                #textSample$testingNews,
                                                #textSample$testingTwitter
                                                ), size = 100))

# Extract sentences
myTestCorpus <- as.data.frame(unlist(tokenize(TestCorpus, 
         what = "sentence")))
names(myTestCorpus) <- c("sentence")
myTestCorpus$sentence <- as.character(myTestCorpus$sentence)

# Extract last word
myTestCorpus$inputText <- gsub(pattern = '\\(|\\)|\\"|:|\\.{3}$|,$|\\?+$|\\!+$|\\.+$|-$| +$', replacement = "", x = myTestCorpus$sentence)
myTestCorpus$inputText <- gsub(pattern = '\\(|\\)|\\"|:|\\.{3}$|,$|\\?+$|\\!+$|\\.+$|-$| +$', replacement = "", x = myTestCorpus$inputText)
myTestCorpus$inputText <- gsub(pattern = '\\(|\\)|\\"|:|\\.{3}$|,$|\\?+$|\\!+$|\\.+$|-$| +$', replacement = "", x = myTestCorpus$inputText)



myTestCorpus$lastWord <-  gsub(pattern = "(.*)\\s((\\w|\\w.\\w)+)$", replacement = "\\2", x = myTestCorpus$inputText) 
myTestCorpus$inputText <-  gsub(pattern = "(.*)\\s((\\w|\\w.\\w)+)$", replacement = "\\1", x = myTestCorpus$inputText) 


## =============================================================================
##      Make predictions
## =============================================================================
myTestCorpus$prediction1 <- ""
myTestCorpus$prediction2 <- ""
myTestCorpus$prediction3 <- ""
myTestCorpus$freqLastWord <- 0
myTestCorpus$predDuration <- 0
myTestCorpus$isPredicted3 <- FALSE
myTestCorpus$isPredicted7 <- FALSE




for (i in 1:nrow(myTestCorpus)) {
        message("::::::::")
        message(i)
        message("::::::::")
        
        t0 <- Sys.time()
        a <- predictNextWord(inputText = myTestCorpus[i,]$inputText,
                             ngramModel = myModelShinyCompact,
                             myBadWords = badWords,
                             algo = "stupidBackoff"
                             )
        t1 <- Sys.time() 
        myTestCorpus[i,]$predDuration <- as.numeric(t1 - t0)
        
        
        if(nrow(a$answer) > 2){
                myTestCorpus[i,]$prediction1 <- a$answer[1,token]
                myTestCorpus[i,]$prediction2 <- a$answer[2,token]
                myTestCorpus[i,]$prediction3 <- a$answer[3,token]
        }
        if(nrow(a$answer) > 1){
                myTestCorpus[i,]$prediction1 <- a$answer[1,token]
                myTestCorpus[i,]$prediction2 <- a$answer[2,token]
        }
        if(nrow(a$answer) == 1){
                myTestCorpus[i,]$prediction1 <- a$answer[1,token]
        }
        ff <- a$answer[ token %in% myTestCorpus[i,]$lastWord,]
        if(nrow(ff) > 0 ){
                myTestCorpus[i,]$freqLastWord <- ff[,freq.Sum]}
        
        if(myTestCorpus[i,]$lastWord %in% c(myTestCorpus[i,]$prediction1, myTestCorpus[i,]$prediction2, myTestCorpus[i,]$prediction1)){
                myTestCorpus[i,]$isPredicted3 <- TRUE
        }
        if(myTestCorpus[i,]$lastWord %in% a$answer[1:7,token]){
                myTestCorpus[i,]$isPredicted7 <- TRUE
        }
        
}

