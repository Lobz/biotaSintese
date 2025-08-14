
taxonRanks <- c("form", "variety", "subspecies", "species", "genus", "family",
               "order", "class", "phylum", "kingdom")

as.taxon.rank <- function(x) factor(x, levels = taxonRanks, ordered=TRUE)
