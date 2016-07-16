# server.R


# Main libs
# ----------
source('./loadMainLibraries.R', chdir = TRUE)
# Source R sripts and functions
source('./tokenizerFunctionsShiny.R', chdir = TRUE)
source('./predictFunction.R', chdir = TRUE)

# Bad words
# ---------
badWordsUrl <- "./swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]


## =============================================================================
##      Load Final Model
## =============================================================================
#load(file = paste('../Models/modelCompact_',sampleSizeM,'.Rda',sep=''))
load(file = './modelShinyCompact_0.75_0.4.Rda')



shinyServer(
        function(input, output) {
                
                output$predictedText <- renderText({ 
                        
                        
                        if (str_length(input$myInputText) > 0 ){
                                gr <- pickMyGramHash(input$myInputText, stpW = badWords)
                        
                                answer <- predictNextWord(
                                        inputText = input$myInputText,
                                        ngramModel = myModelShinyCompact,
                                        myBadWords = badWords,
                                        algo = "stupidBackoff")
                        
                                myAnswer <- paste(
                                        answer$answer[1:min(7, nrow(answer$answer)),token],
                                        collapse = " / "
                                      )
                        } else {
                                myAnswer <- ""
                                }
                        myAnswer
                        })
                
        }
)
