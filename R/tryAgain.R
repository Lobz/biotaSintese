#' Try Again
#'
#' @description Try to run some function on a data.frame again, but only on selected rows, then merge back to the original data.frame.
#'
#' @param x A data.frame
#' @param condition A function that checks which lines must be tried again
#' @param FUN A function
#'
#' @details You may pass aditional parameters to the function
tryAgain <- function(x, condition, FUN, ...) {
    unmatched <- condition(x)
    if(!any(unmatched)) {
        print("No lines satisfy condition")
        return(x)
    }

    unmatched <- which(unmatched)
    rematch <- x[unmatched, ]
    rematch <- FUN(rematch, ...)
    rematched <- !condition(rematch)
    if(!any(rematched)) {
        print("Retrying wielded no results")
        return(x)
    } else {
        print(paste("Replaced", sum(rematched), "lines"))
    }

    newresults <- unmatched[rematched] # let's replace these
    rematch <- rematch[rematched, names(x)]
    x[newresults,] <- rematch
    x
}
