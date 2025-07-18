
getTaxonId <- function(total) {
    # Fix some issues with taxonomy:

    # Some records don't have scientificName for some reason
    noName <- is.na(total$scientificName)
    table(noName)
    # if species is present, use that
    total$scientificName[noName] <- total$species[noName]
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

    # Match scientificName to oficial F&FBR backbone
    total <- formatTax(total)

    # we're gonna try again without author (see issue #170 in plantR)
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, use.author = F)

    # Try again with verbatim
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, tax.name = "verbatimScientificName")

    # And again without author
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, tax.name = "verbatimScientificName", use.author = F)

    # Isolate authorship
    total <- isolateAuthorship(total, overwrite.authorship = FALSE)

    # we're gonna try again without author (see issue #170 in plantR)
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, use.author = F)

    # For records that have authorship inside scientific name, we want to remove that
    total <- tryAgain(total,
        condition = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship.new)
            auth <- gsub("\\)","\\\\)",auth)
            x$tax.notes == "not found" & pairwiseMap(auth, x$scientificName, grepl)
            },
        FUN = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship.new)
            auth <- gsub("\\)","\\\\)",auth)
            x$scientificName <- str_squish(pairwiseMap(auth, x$scientificName, function(x,y) sub(x, "", y)))
            x$scientificName <- sub(", \\d+","",x$scientificName)
            x <- formatTax(x)
            x
        })

    total <- tryAgain(total,
        condition = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship)
            auth <- gsub("\\)","\\\\)",auth)
            x$tax.notes == "not found" & pairwiseMap(auth, x$scientificName, grepl)
            },
        FUN = function(x) {
            auth <- gsub("\\(","\\\\(",x$scientificNameAuthorship)
            auth <- gsub("\\)","\\\\)",auth)
            x$scientificName <- str_squish(pairwiseMap(auth, x$scientificName, function(x,y) sub(x, "", y)))
            x$scientificName <- sub(", \\d+","",x$scientificName)
            x <- formatTax(x)
            x
        })


    # Isolate authorship
    total <- tryAgain(total, function(x) x$tax.notes == "not found", function(x) {formatTax(isolateAuthorship(x))})

    # What's still unmatched? Genus rank
    total <- tryAgain(total, condition = function(x) x$tax.notes == "not found"& x$taxonRank=="genus", FUN = formatTax, tax.name = "genus")

    # What's still unmatched? Vars and subspecies
    total <- tryAgain(total,
        condition = function(x) x$tax.notes == "not found" & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        })

    # What's still unmatched? Try again with less rigor?
    # total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, sug.dist=0.8 )

    total
}
