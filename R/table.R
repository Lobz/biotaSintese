# Table function to always include NAs
tab <- function(...) { sort(table(..., useNA="always")) }
