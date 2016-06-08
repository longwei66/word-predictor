## =============================================================================
##      Getting Data
##
##      - 0. Parameters
##      - 1. Download dataset if it does not exist
##      - 2. Unzip
##      - 3. Load in R
##      - 4. Make a % random sample
##      - 5. Save Robjects & sample files
##      - 6. Clean workspace
## =============================================================================


## -----------------------------------------------------------------------------
## 0. Parameters
## -----------------------------------------------------------------------------
message("0.... define parameters")


## -----------------------------------------------------------------------------
## 1. Download
## -----------------------------------------------------------------------------
message("1.... download data")

#' downloadIF
#' Function to download if file is not existing
#' 
#' @param url 
#' @param file 
#' @param folder 
#'
#' @return
#' @export
#'
#' @examples
downloadIF <- function(url, file, folder) {
        # generate full file name
        fileName <- paste(folder, file, sep = "/")
        # Download only if file does not exists
        if (!file.exists(fileName)) {
                download.file(
                        url = url,
                        destfile = fileName,
                        method = "wget",
                        quiet = FALSE,
                        mode = "w",
                        cacheOK = TRUE
                )
                message("file downloaded")
        } else {
                message("file exists")
        }
}

## Download the file
downloadIF(dataSourceUrl, fileName, dirName)

## -----------------------------------------------------------------------------
## 2. Unzip
## -----------------------------------------------------------------------------
message("2.... Unzip data (skipped)")

## unzip data in data directory
#message("2.... Unzip data
#unzip(zipfile = paste(dirName,fileName,sep = "/"), exdir = dirName)



## -----------------------------------------------------------------------------
## 3. Load the data
## -----------------------------------------------------------------------------
message("3.... Load in R")

## Create data frame to store details about data
englishData <- data.frame(fileName = list.files(finalDataDir))

## Feed the data frame for blogs, news, twitter
englishData$fileUrl <- paste(finalDataDir, englishData$fileName, sep = "/")
englishData$fileSizeMo <- file.size(englishData$fileUrl) / 1024^2
englishData$nbLines <- sapply(englishData$fileUrl, countLines)
englishData$name <- c("blogs","news","twitter")

## Add sample infos
englishData$fileNameSample <- c("en_US.blogs_sample.txt","en_US.news_sample.txt","en_US.twitter_sample.txt")
englishData$fileUrlSample <- paste("../../data/final/en_US_sample",englishData$fileNameSample,sep="/")

## Load whole data
twitter <- readFile(englishData[englishData$name == "twitter",]$fileUrl)
news <- readFile(englishData[englishData$name == "news",]$fileUrl)
blogs <- readFile(englishData[englishData$name == "blogs",]$fileUrl)
# Save
english <- list(details = englishData, data = list(twitter = twitter,news = news, blogs = blogs))






## -----------------------------------------------------------------------------
## 4. Make a sample
## -----------------------------------------------------------------------------
message("4.... Make a sample")

## Take a % Random sample of data
englishData$samplePerc <- samplePerc
twitterSample <- takeSampleVector(twitter, samplePerc,seed = 1234)
blogsSample <- takeSampleVector(blogs, samplePerc, seed = 1234)
newsSample <- takeSampleVector(news, samplePerc, seed = 1234)
englishSample <- list(details = englishData, data = list(twitter = twitterSample,news = newsSample, blogs = blogsSample), samplePerc = samplePerc)

## -----------------------------------------------------------------------------
## 5. Save Robjects and sample data
## -----------------------------------------------------------------------------
message("5.... Save R objects and file")

## Save as R object the Main data
save(english, file = en_US_Rda)

save(englishSample, file = en_US_Sample_Rda)
## Save sample files
write(x = twitterSample, file = englishData[englishData$name == "twitter",]$fileUrlSample)
write(x = newsSample, file = englishData[englishData$name == "news",]$fileUrlSample)
write(x = blogsSample, file = englishData[englishData$name == "blogs",]$fileUrlSample)

## -----------------------------------------------------------------------------
## 6. Save Robjects and sample data
## -----------------------------------------------------------------------------
message("6.... Clean working space")
rm(twitter,news,blogs,english)
rm(englishSample, blogsSample,newsSample,twitterSample)
