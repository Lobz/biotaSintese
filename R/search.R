#' Search for a string in location fields of a data.frame
#' @param pattern Pattern to lookup
#' @param corpus A data.frame with locality information
searchLoc <- function(pattern, corpus) {
    grepl(pattern, x = paste(corpus$municipality, corpus$locality), ignore.case = TRUE, perl = TRUE)
}