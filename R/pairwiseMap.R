#' Pairwise Map
#'
#' Applies a function to each pair of members of two vectors
#'
pairwiseMap <- function(x, y, FUN, ...) {
    if("list" %in% class(x) & "list" %in% class(y)) {
        lapply(1:length(x), function(i) {FUN(x[[i]], y[[i]], ...)})
    }
    else {
        sapply(1:length(x), function(i) {FUN(x[i], y[i], ...)})
    }
}
