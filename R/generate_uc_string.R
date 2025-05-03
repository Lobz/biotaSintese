#' Generate UC string
#'
#' @importFrom plantR rmLatin
generate_uc_string <- function(x) {
    x <- tolower(x)
    x <- gsub(" d[oae]s? ",",? ( d[oae]s?)?",x)
}
