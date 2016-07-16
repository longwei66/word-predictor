## =============================================================================
##
##              Function to predict next words based on a text input
##
##              0.  
##              1.  
##              2. 
##
## =============================================================================


## Debug
# badWordsUrl <- "./../data/swearWords.txt"
# badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]
# load(file = './Models/modelShinyCompact_0.75_0.4.Rda')
# load(file = './Models/modelCompact_0.75.Rda')
# myModel <- myModelShinyCompact
# ngramModel <- myModel
# myBadWords <- badWords
# inputText <- "I am going back home after sch"

predictNextWord <- function(inputText, ngramModel, myBadWords, algo = "basic"){
        
        ## ---------------------------------------------------------------------
        ## Extract n-gram from input text and hash
        ## This returns 
        ## gr$myWords, a vector of input words
        ## gr$myInputGrams 4,3,2,1 grams of the end of input text
        ## gr$myQuadGramH... has of 4,3,2,1 grams of the end of input
        ## ---------------------------------------------------------------------
        gr <- pickMyGramHash(inputText, stpW = myBadWords)
        
        message(paste(gr$myInputGrams, collapse = " - "))
        
        ## ---------------------------------------------------------------------
        ## Get the matching 5,4,3,2,1 grams
        ## ---------------------------------------------------------------------
        ## Look for matching quint grams
        # q5 <- ngramModel[(index == gr$myQuadGramH & type == 5), ]
        # Count the frequency of root 4 Gram
        # cnt4 <- countMyToken(myGram = gr$myInput[1], ngramModel, 4)
        
        ## Look for matching quad grams
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        q4 <- ngramModel[(index == gr$myTriGramH & type == 4), ]
        r4 <- nrow(q4)
        ## Count the frequency of matching root 3 gram
        cnt3 <- countMyToken(myGram = gr$myInput[2], ngramModel, 3)
        
        ## Look for matching tri grams
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        q3 <- ngramModel[(index == gr$myBiGramH & type == 3), ]
        r3 <- nrow(q3)
        ## Count the frequency of matching root 2 gram
        cnt2 <- countMyToken(myGram = gr$myInput[3], ngramModel, 2)
        
        ## Look for matching bi grams
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        q2 <- ngramModel[(index == gr$myUniGramH & type == 2), ]
        r2 <- nrow(q2)
        
        ## Look for matching unigram
        ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        q1 <- ngramModel[(index == gr$myUniGramH & type == 1), ]
        r1 <- nrow(q1)
        voccSize <- nrow(ngramModel)
        
        ## init final choice which is the flat list of unigrams
        q0 <- NULL
        
        if(nrow(q1) > 0) {
                cnt1 <- ngramModel[(index == gr$myUniGramH & type == 1), freq]
        } else {
                # If data is missing for the n-1 gram use the replace by 1 strategy
                cnt1 <- 1
        }
        
        ## make a choice for algo
        ## ---------------------------------------------------------------------
        ## Algo : basic
        ## ---------------------------------------------------------------------
        if(algo == "basic"){
                # back-off :
                # use quad grams, if no evidence, use tri grams, .. bi grams, unigrams
                
                ## look for matching quadgrams
                if ( r4 > 0) {
                        q4[,freq := round(freq / cnt3 * 100,5)]
                } else {
                        ## Look for matching tri grams
                        if ( r3 > 0 ) {
                                q3[,freq := round(freq / cnt2 *100,5)]
                        } else {
                                ## Look for matching bii grams
                                if ( r2 > 0) {
                                        q2[,freq := round(freq / cnt1 *100,5)]
                                } else {
                                        ## propose unigrams
                                        q0 <- ngramModel[type == 1, ]
                                        q0[1:10,]
                                }
                        }
                }
        }
        
        
        ## ---------------------------------------------------------------------
        ## Algo : stupid backoff
        ## ---------------------------------------------------------------------
        if(algo == "stupidBackoff"){
                # stupid back-off :
                # if tri gram, use it otherwise use 0.4 the brigram...
                # then use unigram / N (vocc size)
                # does not give back proba but scores
                
                ## -----------------------------------------------------
                ## Parse the nGrams model
                ## -----------------------------------------------------
                ## look for matching quadgrams
                if ( r4 > 1) {
                        q4[,freq := round(freq / cnt3 * 100 * 0.5,5)]
                        q3[,freq := round(freq / cnt2 *100 * 0.3,5)]
                        q2[,freq := round(freq / cnt1 *100 * 0.2,5)]
                        #q1[,freq := round(freq / voccSize *100,5)]
                } else {
                        ## Look for matching tri grams
                        if ( r3 > 1 ) {
                                q3[,freq := round(freq / cnt2 *100 * 0.7,5)]
                                q2[,freq := round(freq / cnt1 *100 * 0.3,5)]
                                #q1[,freq := round(freq / voccSize *100,5)]
                        } else {
                                ## Look for matching bii grams
                                if ( r2 > 1) {
                                        q2[,freq := round(freq / cnt1 *100,5)]
                                        #q1[,freq := round(freq / voccSize *100,5)]
                                } else {
                                        
                                        ## propose unigrams
                                        q0 <- ngramModel[type == 1, ]
                                        q0[1:10,]      
                                        
                                }
                        }
                }
                ## -----------------------------------------------------
                ## Add the different tokens with their weight
                ## -----------------------------------------------------
                ## for final answer add probability for each tokens
                answer  <- rbindlist(list(q4,q3,q2,q0))
                answer[, c(
                        "index",
                        "type"
                ) := NULL]
                ## sum the freq by token to create the weight vector
                answer <- answer[,.(freq.Sum = sum(freq)),by=token]
                
                inputBiGrams <- extract_bigrams(inputText, myBadWords)
                inputUniGrams <- extract_unigrams(inputText, myBadWords)
                
                return(list(
                        gr = gr,
                        q4 = q4,
                        q3 = q3,
                        q2 = q2,
                        q1 = q1,
                        q0 = q0,
                        answer = answer,
                        biGrams = inputBiGrams,
                        uniGrams = inputUniGrams
                ))
                
                
                # interpolation :
                # mix quad grams, tri grams, bi grams, unigrams (summ of alphas = 1)
                
        }
}



predictNextWordFast <- function(inputText, ngramModel, myBadWords){
        
        # Extract n-gram from input text and hash
        gr <- pickMyGramHash(inputText, stpW = myBadWords)
        
        ## Look for matching quad grams
        q4 <- ngramModel[(index == gr$myTriGram & type == 4), ]
        cnt <- countMyToken(myGram = gr$myInput[1], ngramModel, 3)
        q4[,freq := round(freq / cnt * 100,5)]
        
        ## Look for matching tri grams
        q3 <- ngramModel[(index == gr$myBiGram & type == 3), ]
        cnt <- countMyToken(myGram = gr$myInput[2], ngramModel, 2)
        ## use alpha of 0.4 for stupid back-off
        q3[,freq := round(freq / cnt * 0.4 *100,5)]
        
        ## Look for matching bi grams
        q2 <- ngramModel[(index == gr$myUniGram & type == 2), ]
        cnt <- ngramModel[(index == gr$myUniGram & type == 1), freq]
        ## use alpha of 0.4 for stupid back-off
        q2[,freq := round(freq / cnt * 0.4 * 0.4 *100,5)]
        
        ## Look for matching unigram
        q1 <- ngramModel[(index == gr$myUniGram & type == 1), ]
        q1[,freq := round(freq / cnt * 0.4 * 0.4 * 0.4 *100,5)]
        

        ## for final answer add probability for each tokens
        answer  <- rbindlist(list(q4,q3,q2))
        answer[, c(
                "index",
                "type"
        ) := NULL]
        answer <- answer[,.(freq.max = max(freq)),by=token]
        
        # answer[,.(prob = sum(freq)), by = token]
        return(list(
                q4 = q4,
                q3 = q3,
                q2 = q2,
                q1 = q1,
                answer = answer
        ))
}