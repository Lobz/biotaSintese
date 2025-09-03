
getTaxonId <- function(total) {
    tax.names= c("scientificName", "scientificNameAuthorship")
    tax.names.new = c("scientificName.new", "scientificNameAuthorship.new")
    tax.names.v= c("verbatimScientificName", "scientificNameAuthorship")
    tax.names.v.new = c("verbatimScientificName.new", "scientificNameAuthorship.new")
    # Fix some issues with taxonomy:

    # Some records don't have scientificName for some reason
    noName1 <- is.na(total$scientificName)
    table(noName1)
    # if species is present, use that
    total$scientificName[noName1] <- total$species[noName1]
    noName <- is.na(total$scientificName)
    table(noName)
    # else, use genus
    total$scientificName[noName] <- total$genus[noName]
    noName <- is.na(total$scientificName)
    table(noName)
    # last resort, use family
    total$scientificName[noName] <- total$family[noName]
    noName <- is.na(total$scientificName)
    table(noName)

    # Which records have not been matched?
    not_found <- function(x) x$tax.notes == "not found"

    # Match scientificName to oficial F&FBR backbone
    if("tax.notes" %in% names(total)) {
        total <- tryAgain(total, not_found, prepSpecies, use.authors = F)
    } else {
        total <- prepSpecies(total, use.authors = F)
    }

    # we're gonna try again with author (see issue #170 in plantR)
    total <- tryAgain(total, not_found, prepSpecies)

    # Try again with verbatim
    total <- tryAgain(total, not_found, prepSpecies, tax.names = tax.names.v, use.author = F)

    # And again with author
    total <- tryAgain(total, not_found, prepSpecies, tax.name = tax.names.v)

    # Isolate authorship
    total[not_found(total),] <- isolateAuthorship(total[not_found(total),], overwrite.authorship = FALSE)

    # we're gonna try again without author (see issue #170 in plantR)
    total <- tryAgain(total, not_found, prepSpecies, use.author = F)

    # For records that have authorship inside scientific name, we want to remove that
    total <- tryAgain(total,
        condition = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship.new)
            auth <- gsub("\\)","\\\\)",auth)
            not_found(x) & pairwiseMap(auth, x$scientificName, grepl)
            },
        FUN = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship.new)
            auth <- gsub("\\)","\\\\)",auth)
            x$scientificName <- str_squish(pairwiseMap(auth, x$scientificName, function(x,y) sub(x, "", y)))
            x$scientificName <- sub(", \\d+","",x$scientificName)
            x <- prepSpecies(x)
            x
        })

    total <- tryAgain(total,
        condition = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship)
            auth <- gsub("\\)","\\\\)",auth)
            not_found(x) & pairwiseMap(auth, x$scientificName, grepl)
            },
        FUN = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship)
            auth <- gsub("\\)","\\\\)",auth)
            x$scientificName <- str_squish(pairwiseMap(auth, x$scientificName, function(x,y) sub(x, "", y)))
            x$scientificName <- sub(", \\d+","",x$scientificName)
            x <- prepSpecies(x)
            x
        })


    # Isolate authorship
    total <- tryAgain(total, not_found, function(x) {prepSpecies(isolateAuthorship(x))})

    # What's still unmatched? Genus rank
    total <- tryAgain(total, condition = function(x) not_found(x) & x$taxonRank=="genus", FUN = prepSpecies, tax.name = "genus")

    # What's still unmatched? Vars and subspecies
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
            x <- prepSpecies(x)
            x$scientificName <- saved
            x
        })
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 subsp. ", x$scientificName)
            x <- prepSpecies(x)
            x$scientificName <- saved
            x
        })
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 f. ", x$scientificName)
            x <- prepSpecies(x)
            x$scientificName <- saved
            x
        })

    # What's still unmatched? Try again with less rigor?
    # total <- tryAgain(total, function(x) x$tax.notes == "not found", prepSpecies, sug.dist=0.8 )

    # Finally, if something is still unmatched, give up and match higher taxon rank
    total <- tryAgain(total,
        condition = function(x) {not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName)},
        FUN = function(x) {
            x$scientificName.new <- sub("(^\\w+ \\w+).*", "\\1", x$scientificName)
            x <- prepSpecies(x, )
            x
        })


    total formatTax()
}
