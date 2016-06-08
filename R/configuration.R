
## Data Source
dataSourceUrl <-
        "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "Coursera-SwiftKey.zip"
dirName <-  "../../data"

## configure data folder names we will use
finalDataDir <- "../../data/final/en_US/"
sampleDataDir <- "../../data/final/en_US_sample/"

## configure the sample %
samplePerc <- 0.001

## Rda object path/file
en_US_Rda <- "../../Robjects/en_US.Rda"
en_US_Sample_Rda <- "../../Robjects/en_US_Sample.Rda"

## Bad words list
badWordsUrl <- "../../data/bad-words.txt"