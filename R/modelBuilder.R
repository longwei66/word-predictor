## Basic configuration
setwd('./R/')
sampleSizeM <- 0.75
save(sampleSizeM, file = './sampleSizeM.Rda')


## =============================================================================
##      Main Steps
##
##      I. Make sample Text
##              - use the function : makeTextSample
##
##      II. Make tokens
##              - use the function : makeTokens
##              - generate object : tokens
##              - saved in ../../data/Rda/tokens_sampleSizeM.Rda'
##
##      III. Make 1-grams
##              - use the script : './make5Gram.R'
##              - generate object : uniGrams
##              - saved in '../../data/Rda/sw/uniGrams_sampleSizeM.Rda
##
##      IV. Make 2-grams
##              - use the script : './make2Gram.R'
##              - generate object : biGrams
##              - saved in '../../data/Rda/sw/biGrams_sampleSizeM.Rda
##      V. Make 3-grams
##              - use the script : './make3Gram.R'
##              - generate object : triGrams
##              - saved in '../../data/Rda/sw/triGrams_sampleSizeM.Rda
##
##      VI. Make 4-grams
##              - use the script : './make4Gram.R'
##              - generate object : quadGrams
##              - saved in '../../data/Rda/sw/quadGrams_sampleSizeM.Rda
##
##      VII. Make 5-grams
##              - use the script :'./make5Gram.R'
##              - generate object : quintGrams
##              - saved in '../../data/Rda/sw/quintGrams_sampleSizeM.Rda
##
##      VIII. Combine and compress models
##              - use the script : ./combineAndCompressModel.R'
##              - generate object : uniGrams, biGrams, triGrams, quadGrams, quintGrams
##              - saved with their respectives names in '../Models/1-5_Grams_sampleSizeM_with_sw_limit1.Rda
##              - generate object : myModel
##              - saved in : '../Models/modelCompact_.Rda
##
##      IX. Make for model for Shiny APP
##              - use the script : './prepareModelforShiny.R'
##              - generate object : myModelShinyCompact
##              - saved in '../Models/myModelShinyCompact_with_sw_limit1.Rda (uncompressed version)
##
##
##
## =============================================================================






## =============================================================================
##      I. Make sample Text
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

## Extract a Sample of the text Database
textSample <- makeTextSample(sampleSizeM = sampleSizeM)
rm(list = ls(all=TRUE))
.rs.restartR()


## =============================================================================
##      II. Make tokens
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')

load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/sampleText_', sampleSizeM, '_.Rda', sep = ""))

myText <- c(
        paste(textSample$blogsSample, collapse = " "),
        paste(textSample$newsSample, collapse = " "),
        paste(textSample$twitterSample, collapse = "")
)
tokens <- makeTokens(myText = myText, sampleSizeM = sampleSizeM)
rm(list = ls(all=TRUE))
.rs.restartR()



## =============================================================================
##      III. Make 1-grams
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/tokens_', sampleSizeM, '.Rda', sep = ""))

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

source('./make1Gram.R')
rm(list = ls(all=TRUE))
.rs.restartR()



## =============================================================================
##      IV. Make 2-grams
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/tokens_', sampleSizeM, '.Rda', sep = ""))

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

source('./make2Gram.R')
rm(list = ls(all=TRUE))
.rs.restartR()


## =============================================================================
##      V. Make 3-grams
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/tokens_', sampleSizeM, '.Rda', sep = ""))

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

source('./make3Gram.R')
rm(list = ls(all=TRUE))
.rs.restartR()


## =============================================================================
##      VI. Make 4-grams
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/tokens_', sampleSizeM, '.Rda', sep = ""))

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

source('./make4Gram.R')
rm(list = ls(all=TRUE))
.rs.restartR()


## =============================================================================
##      VII. Make 5-grams
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Load Sample text
load(paste('../../data/Rda/tokens_', sampleSizeM, '.Rda', sep = ""))

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

source('./make5Gram.R')
rm(list = ls(all=TRUE))
.rs.restartR()



## =============================================================================
##      VIII. Combine and compress models
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
load('./sampleSizeM.Rda')

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]
minFreqRatio <- 0
source('./combineAndCompressModel.R')
rm(list = ls(all=TRUE))
.rs.restartR()


## =============================================================================
##      IX. Make for model for Shiny APP
## =============================================================================
# Clear objects in memory
rm(list = ls(all=TRUE))
# Garbage collection (memory)
gg <- gc(reset = TRUE)
# Main libs
source('./loadMainLibraries.R')
# Source R sripts and functions
source('./tokenizerFunctions.R')
source('./predictFunction.R')
load('./sampleSizeM.Rda')

# Bad words
badWordsUrl <- "../../data/swearWords.txt"
badWords <- read.csv(badWordsUrl,stringsAsFactors = FALSE)[,1]

# Load Compact model
load(file = paste('../Models/modelCompact_',sampleSizeM,'.Rda',sep=''))

keepGramRatio <- 0.7
source('./prepareModelforShiny.R')
rm(list = ls(all=TRUE))
.rs.restartR()


