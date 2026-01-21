#' Search for a string in location fields of a data.frame
#' @param pattern Pattern to lookup
#' @param corpus A data.frame with locality information
searchLoc <- function(pattern, corpus) {
    x <- grepl(pattern, x = corpus$municipality, ignore.case = TRUE, perl = TRUE)
    y <- grepl(pattern, x = corpus$locality, ignore.case = TRUE, perl = TRUE)
    x | y
}
