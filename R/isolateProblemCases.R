isolateProblemCases <- function(x, FUN, breaks = 10, ...) {
    n <- nrow(x)
    groups <- cut(1:n, breaks)
    l_orig <- split(x, groups)
    l_prob <- sapply(l_orig, function(x1) {
        tryCatch(
            {FUN(x1); FALSE},
            error = function(e) {return(TRUE)}
        )
    })
    l_prob <- l_orig[l_prob]
    # if(recursive) {
        # l_prob <- lapply(l_prob, FUN = function(x2) isolateProblemCases(x2, FUN=FUN, breaks = breaks, recursive = F))
    # }
    x <- dplyr::bind_rows(l_prob)
    x
}
