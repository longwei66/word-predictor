## =============================================================================
##      Prepare the model for Shiny APP
##      Try to compress as much as possible
## =============================================================================

## Sum of frequency keep

keepGram <- list(
        #ratio1 = 0.99 * keepGramRatio,
        ratio1 = 1,
        ratio2 = 0.7,
        ratio3 = 0.65,
        ratio4 = 0.35
) 
# 0.35 => 1, 0.7, 0.65, 0.35
# 0.6 => 1,0.7,0.8,0.6
# 0.9 => 1,0.99,0.95,0.9
# 0.4 => 1, 0.8, 0.6, 0.4


## Remove quintGrams
object.size(myModel) / 1024 ^2
myModelShiny <- myModel[type != 5,]
object.size(myModelShiny) / 1024 ^2


## =============================================================================
##              Plots frequencies in cumed
## =============================================================================

## -----------------------------------------------------------------------------
## work the uniGrams
## -----------------------------------------------------------------------------
gr1 <- myModelShiny[type == 1]
object.size(gr1) / 1024 ^2
gr1 <- gr1[, cumFreq := cumsum(freq)]
n <- nrow(gr1)
X <- sample(x = 1:n, size = 0.01*n)
Y <- gr1[X,cumFreq]
plot(X,Y)
nr <- nrow(gr1)
max <- gr1[nrow(gr1), cumFreq]
th <- nrow(gr1[ cumFreq <= keepGram$ratio1 * max ,])
n
th
th / n

## Keep xx% of the cumed freq terms
gr1Compact <- gr1[ cumFreq <= keepGram$ratio1  * max ,]
object.size(gr1Compact) / 1024 ^2
rm(gr1)

## -----------------------------------------------------------------------------
## work the biGrams
## -----------------------------------------------------------------------------
gr2 <- myModelShiny[type == 2]
object.size(gr2) / 1024 ^2
gr2 <- gr2[, cumFreq := cumsum(freq)]
n <- nrow(gr2)
X <- sample(x = 1:n, size = 0.01*n)
Y <- gr2[X,cumFreq]
plot(X,Y)
nr <- nrow(gr2)
max <- gr2[nrow(gr2), cumFreq]
th <- nrow(gr2[ cumFreq <= keepGram$ratio2 * max ,])
n
th
th / n

## Keep keepGramRatio% of the cumed freq terms
gr2Compact <- gr2[ cumFreq <= keepGram$ratio2 * max ,]
object.size(gr2Compact) / 1024 ^2
rm(gr2)


## -----------------------------------------------------------------------------
## work the triGrams
## -----------------------------------------------------------------------------
gr3 <- myModelShiny[type == 3]
object.size(gr3) / 1024 ^2
gr3 <- gr3[, cumFreq := cumsum(freq)]
n <- nrow(gr3)
X <- sample(x = 1:n, size = 0.01*n)
Y <- gr3[X,cumFreq]
plot(X,Y)
nr <- nrow(gr3)
max <- gr3[nrow(gr3), cumFreq]
th <- nrow(gr3[ cumFreq <= keepGram$ratio3 * max ,])
n
th
th / n

## Keep xxx% of the cumed freq terms
gr3Compact <- gr3[ cumFreq <= keepGram$ratio3 * max ,]
object.size(gr3Compact) / 1024 ^2
rm(gr3)

## -----------------------------------------------------------------------------
## work the quadGrams
## -----------------------------------------------------------------------------
gr4 <- myModelShiny[type == 4]
object.size(gr4) / 1024 ^2
gr4 <- gr4[, cumFreq := cumsum(freq)]
n <- nrow(gr4)
X <- sample(x = 1:n, size = 0.01*n)
Y <- gr4[X,cumFreq]
plot(X,Y)
nr <- nrow(gr4)
max <- gr4[nrow(gr4), cumFreq]
th <- nrow(gr4[ cumFreq <= keepGram$ratio4 * max ,])
n
th
th / n

## Keep 90% of the cumed freq terms
gr4Compact <- gr4[ cumFreq <= keepGram$ratio4 * max ,]
object.size(gr4Compact) / 1024 ^2
rm(gr4)


## Merge the models
object.size(myModelShiny) / 1024 ^2
myModelShinyCompact <- rbindlist(list(gr1Compact,gr2Compact,gr3Compact,gr4Compact))
object.size(myModelShinyCompact) / 1024 ^2

## Removes the last column
myModelShinyCompact[,cumFreq := NULL]

## Save the model
save(myModelShinyCompact, file = paste('../Models/modelShinyCompact_',sampleSizeM,'_',keepGramRatio,'.Rda',sep=''), compress = FALSE)
# 

## =============================================================================
##              Make tests
## =============================================================================

# Set a tracker data frame to record operation time
tracker <- data.frame(operation = "init", time = Sys.time(), free.ram.Mo = getRam())
predictNextWordFast("I am a lonely hiatus", myModel, badWords)
tracker <- rbind(tracker, data.frame(operation = "main model", time = Sys.time(), free.ram.Mo = getRam()))
predictNextWordFast("I am a lonely hiatus", myModelShiny, badWords)
tracker <- rbind(tracker, data.frame(operation = "shiny model", time = Sys.time(), free.ram.Mo = getRam()))
predictNextWordFast("I am a lonely hiatus", myModelShinyCompact, badWords)
tracker <- rbind(tracker, data.frame(operation = "shiny model compact", time = Sys.time(), free.ram.Mo = getRam()))

tracker$op.duration <- round(c(0,(tracker[2:nrow(tracker),]$time - tracker[1:(nrow(tracker)-1),]$time)),1)
tracker$total.duration <- round(tracker$time - tracker[1,]$time,1)
tracker$total.duration.min <- round(tracker$total.duration / 60,1)