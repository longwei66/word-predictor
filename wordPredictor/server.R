# server.R


# Main libs
# ----------
source('../R/loadMainLibraries.R', chdir = TRUE)
# Source R sripts and functions
source('../R/tokenizerFunctions.R', chdir = TRUE)
source('../R/predictFunction.R', chdir = TRUE)


sampleSizeM <- 0.75 # Available models are 0.75, 0.5, 0.15, 0.05 and 0.03, 0.01
## Make a persistent record of this variable in case we have to restart R Session
save(sampleSizeM, file = '../R/sampleSizeM.Rda')
load('../R/sampleSizeM.Rda')

# Bad words
# ---------
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]


## =============================================================================
##      Load Final Model
## =============================================================================
#load(file = paste('../Models/modelCompact_',sampleSizeM,'.Rda',sep=''))
load(file = '../Models/modelShinyCompact_0.75_0.7.Rda')
myModel <- myModelShinyCompact

shinyServer(
        function(input, output) {
                
                output$predictedText <- renderText({ 
                        
                        ## Debug message
                        message(":: model size ::")
                        message(nrow(myModel))
                        message(":: bad words removed ::")
                        message(length(badWords))
                        
                        if (str_length(input$myInputText) > 0 ){
                                gr <- pickMyGramHash(input$myInputText, stpW = badWords)
                                
                                ## Debug Message
                                message(gr$myInputGrams[1])
                                #message(input$myInputText)
                                
                                answer <- predictNextWord(
                                        inputText = input$myInputText,
                                        ngramModel = myModel,
                                        myBadWords = badWords,
                                        algo = "stupidBackoff")
                                
                                # debug
                                #answer <- predictNextWord(inputText = "I will go to New ", ngramModel = myModel, myBadWords = badWords, removeSW = FALSE, algo = "basic")
                                
                                myAnswer <- paste(
                                        answer$answer[1:min(10, nrow(answer$answer)),token],
                                        collapse = " / "
                                      )
                        } else {
                                myAnswer <- ""
                                }
                        myAnswer
                        })
                
        }
)
