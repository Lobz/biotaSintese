#' Search for a string in location fields of a data.frame
#' @param pattern Pattern to lookup
#' @param corpus A data.frame with locality information
searchLoc <- function(pattern, corpus) {
    x <- grepl(pattern, x = rmLatin(corpus$municipality), ignore.case = TRUE, perl = TRUE)
    y <- grepl(pattern, x = rmLatin(corpus$locality), ignore.case = TRUE, perl = TRUE)
    x | y
}
