#' Isolate Problem Cases
#'
#' @importFrom parallel parSapply detectCores makeCluster
isolateProblemCases <- function(x, FUN, breaks = 10, no_cores = detectCores() -1, ...) {

    # Initiate cluster
    cl <- makeCluster(no_cores)

    n <- nrow(x)
    groups <- cut(1:n, breaks)
    l_orig <- split(x, groups)
    if(no_cores > 1) {
        l_prob <- parSapply(makeCluster(no_cores), l_orig, function(x1) {
            tryCatch(
                {FUN(x1); FALSE},
                error = function(e) {print(e); return(TRUE)}
            )
        })
    }
    else {
        l_prob <- vapply(l_orig, function(x1) {
            tryCatch(
                {FUN(x1); FALSE},
                error = function(e) {return(TRUE)}
            )
        }, TRUE)
    }
    l_prob <- l_orig[l_prob]
    # if(recursive) {
        # l_prob <- lapply(l_prob, FUN = function(x2) isolateProblemCases(x2, FUN=FUN, breaks = breaks, recursive = F))
    # }
    x <- dplyr::bind_rows(l_prob)
    x
}

loc.cols <- c("country", "stateProvince", "municipality", "locality")
tax.cols <- c("scientificName", "family")