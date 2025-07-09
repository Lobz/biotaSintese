#' Isolate Authorship
#'
#' Extract authorship information from scientific name
#'
#' @param x A data frame containing scientificName information
#' @param save.original.as Name for the column where original scientific name will be saved
#' @param save.original.authorship.as Name for the column where original scientificNameAuthorship will be saved
#' @param tax.name Name for the column with scientificName information in x
#' @param tax.author Name for the column with scientificNameAuthorship information in x
#'
#' @return The x data.frame with corrected information in tax.name and tax.author columns, plus new columns save.original.as and save.original.authorship.as. If these columns are already present in x, they will be overwritten.
#'
#' @importFrom plantR fixAuthors
#'
#' @details This function uses plantR::fixAuthors() to extract authorship information
isolateAuthorship <- function(x,
    save.original.as = "verbatimScientificName",
    tax.name = "scientificName",
    tax.author = "scientificNameAuthorship",
    save.original.authorship.as = "verbatimScientificNameAuthorship") {

    # Create a column to save original name
    x[,save.original.as] <- x[,tax.name]
    x[,save.original.authorship.as] <- x[,tax.author]

    # Isolate authorship from taxon names
    species <- as.character(unique(x[,tax.name]))
    species_split <- fixAuthors(species)

    # Select only those that were corrected
    species_split <- na.omit(species_split)
    fix_these <- x$scientificName %in% species_split$orig.name

    # Info
    print(paste("Isolated authorship for", sum(fix_these), "records."))

    # Merge back
    m <- match(x[fix_these,tax.name], species_split$orig.name)
    x[fix_these, tax.name] <- species_split$tax.name[m]
    x[fix_these, tax.author] <- species_split$tax.author[m]

    x
}
