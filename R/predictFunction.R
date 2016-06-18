## =============================================================================
##
##              Function to predict next words based on a text input
##
##              0.  
##              1.  
##              2. 
##
## =============================================================================


predictNextWord <- function(inputText, ngramModel, myBadWords, removeSW = TRUE){
        

        
        # Extract n-gram from input text and hash
        gr <- pickMyGramHash(inputText, stpW = myBadWords)
        
        ## Look for matching quad grams
        q4 <- myModel[(index == gr$myTriGram & type == 4), ]
        cnt <- countMyToken(myGram = gr$myInput[1], ngramModel, 3)
        q4[,freq := as.integer(round(freq / cnt * 100,0))]
        
        ## Look for matching tri grams
        q3 <- myModel[(index == gr$myBiGram & type == 3), ]
        cnt <- countMyToken(myGram = gr$myInput[2], ngramModel, 2)
        q3[,freq := as.integer(round(freq / cnt * 0.4 *100))]
        
        ## Look for matching bii grams
        q2 <- myModel[(index == gr$myUniGram & type == 2), ]
        cnt <- countMyToken(myGram = gr$myInput[3], ngramModel, 1)
        q2[,freq := as.integer(round(freq / cnt * 0.4 * 0.4 *100))]
        
        ## Look for matching unigram
        q1 <- myModel[(index == gr$myUniGram & type == 1), ]

        if (removeSW){
                sw <- tm::stopwords(kind = "en")
                q4 <- q4[!(token %in% sw), ]
                q3 <- q3[!(token %in% sw), ]
                q2 <- q2[!(token %in% sw), ]
        } 
        answer  <- rbindlist(list(q4,q3,q2))
        answer[, c("index", "type") := NULL]
        answer <- answer[,.(freq.Sum = sum(freq)),by=token]
        
        
        # answer[,.(prob = sum(freq)), by = token]
        return(list(
                gr = gr,
                q4 = q4,
                q3 = q3,
                q2 = q2,
                q1 = q1,
                answer = answer
        ))
}