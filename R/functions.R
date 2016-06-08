## -----------------------------------------------------------------------------
## Functions
## -----------------------------------------------------------------------------

## Read file

#' load the content of the file
#'
#' @param fileUrl 
#'
#' @return fileData the content of the file
#' @export
#'
#' @examples
readFile <- function(fileUrl){
        con <- file(fileUrl, "r")
        fileData <- readLines(con, -1, skipNul = TRUE) 
        close(con) 
        return(fileData)
}


#' takeSampleVector
#' Take a random sample of %percent of a vector
#' 
#' 
#' @param vector 
#' @param percent 
#'
#' @return v a sampled vector
#' @export
#'
#' @examples
takeSampleVector <- function(vector,percent,seed=1234){
        set.seed(seed = seed)
        v <- sample(vector, size = round(percent * length(vector),0))
        return(v)
        }