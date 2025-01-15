# Remove empty columns
#
# This will remove any column from a data.frame that is empty.
remove_empty_cols <- function(x) {
    empties <- sapply(x, function(c){all(is.na(c))})
    x[,empties] <- NULL
    x
}
