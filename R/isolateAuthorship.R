
isolateAuthorship <- function(x, save.original.as = "verbatimScientificName") {

    # Create a column to save original name
    x[,save.original.as] <- x$scientificName

    # Isolate authorship from taxon names that do not have authorship information
    species <- as.character(unique(x$scientificName[is.na(x$scientificNameAuthorship)]))
    species_split <- fixAuthors(species)

    # Select only those that were corrected
    species_split <- na.omit(species_split)
    fix_these <- x$scientificName %in% species_split$orig.name

    # Info
    print(paste("Isolated authorship for", sum(fix_these), "records."))

    # Merge back
    m <- match(x$verbatimScientificName[fix_these], species_split$orig.name)
    x$scientificName[fix_these] <- species_split$tax.name[m]
    x$scientificName[fix_these] <- species_split$tax.name[m]

    x
}
