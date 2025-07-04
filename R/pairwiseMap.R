#' Pairwise Map
#'
#' Applies a function to each pair of members of two vectors
#'
pairwiseMap <- function(x, y, FUN, ...) {
    sapply(1:length(x), function(i) {FUN(x[i], y[i], ...)})
}
