#       0. Configuration
#       I. Load libraries
#       II. Get the data
#       III. Clean the data
#
#



## =============================================================================
##      0. Configuration
## =============================================================================

# Clear objects in memory
rm(list = ls(all=TRUE))
# Set a tracker data frame to record operation time
tracker <- data.frame(operation = "init", time = Sys.time())

## Absolute location of the R scripts
myScriptDir <- "/home/longwei/ownCloud/Projets/Data-Science/coursera/10_Capstone_project/word-predictor/R"
setwd(myScriptDir)

# load file path, ...
source('./configuration.R')

# Set appropriate locale
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# Memory Status
gc()

## =============================================================================
##      I. Load libraries
## =============================================================================

source("./loadLibraries.R")
source('./functions.R')
tracker <- rbind(tracker, data.frame(operation = "loadlibs", time = Sys.time()))
# Memory Status
gc()

## =============================================================================
##      II. Get the data
## =============================================================================
## Remove saved objects
unlink(en_US_Rda)
unlink(en_US_Sample_Rda)

source("./getting-data.R")
## Load the sample data
load(en_US_Sample_Rda)
tracker <- rbind(tracker, data.frame(operation = "get Data", time = Sys.time()))
# Memory Status
gc()

## =============================================================================
##      III. Clean the data
## =============================================================================
source("./cleaning-data.R")
tracker <- rbind(tracker, data.frame(operation = "clean Data", time = Sys.time()))
# Memory Status
gc()



## =============================================================================
##      IV. Analyse the data
## =============================================================================
source("./analyse-data.R")
tracker <- rbind(tracker, data.frame(operation = "analyse Data", time = Sys.time()))
# Memory Status
gc()

