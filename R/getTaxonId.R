
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

    # Which records have not been matched?
    not_found <- function(x) {
        x$tax.notes == "not found" | grepl("not resolved|+1",x$tax.notes)
    }
    found <- function(x) !not_found(x)

    # Match scientificName to oficial F&FBR backbone
    if("tax.notes" %in% names(total)) {
        total <- tryAgain(total, not_found, formatTax, label = "Default formatTax")
    } else {
        total <- formatTax(total)
    }

    # Try again with verbatim
    total <- tryAgain(total, not_found, formatTax, tax.name = "verbatimScientificName", label = "Verbatim")

    # we're gonna try again without author (see issue #170 in plantR)
    # total <- tryAgain(total, not_found, formatTax, use.authors = F)

    # And again with author
    # total <- tryAgain(total, not_found, formatTax, tax.name = "verbatimScientificName", use.author = F)

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
            x <- formatTax(x)
            x
        },
        success_condition = found,
        label = "Removed auth 1")

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
            x <- formatTax(x)
            x
        },
        success_condition = found,
        label = "Removed auth 2")

    # Isolate authorship
    total[not_found(total),] <- isolateAuthorship(total[not_found(total),], overwrite.authorship = FALSE)

    # we're gonna try again without author (see issue #170 in plantR)
    total <- tryAgain(total, not_found, formatTax, label = "Isolated")

    # Isolate authorship
    total <- tryAgain(total, not_found, function(x) {formatTax(isolateAuthorship(x))}, label = "Isolate 2")

    # What's still unmatched? Genus rank
    total <- tryAgain(total, condition = function(x) not_found(x) & x$taxonRank=="genus", FUN = formatTax, tax.name = "genus", label = "Genus")

    # What's still unmatched? Vars and subspecies
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Var.")
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 subsp. ", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Subsp.")
    total <- tryAgain(total,
        condition = function(x) not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 f. ", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "F.")

    # What's still unmatched? Try again with less rigor?
    # total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, sug.dist=0.8 )

    # Finally, if something is still unmatched, give up and match higher taxon rank
    total <- tryAgain(total,
        condition = function(x) {not_found(x) & grepl("\\w+ \\w+ \\w", x$scientificName)},
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(^\\w+ \\w+).*", "\\1", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        },
        success_condition = found,
        label = "Remove infra")

    # validate taxonomist
    total <- validateTax(total, generalist = T)
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)

    # fix missing taxon rank
    total$taxon.rank <- as.taxon.rank(total$taxon.rank)
    table((total$taxon.rank), useNA = "always")
    table(is.na(total$taxon.rank))
    # If there's another entry of the same taxon, get the taxon rank from there
    total <- total[order(total$taxon.rank),]
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
    # If possible, get it from taxonRank
    fix_these <- which(is.na(total$taxon.rank))
    total$taxon.rank[fix_these] <- tolower(total$taxonRank[fix_these])
    # Otherwise, look at scientific name
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    x <- sub(" sp.","",x)
    rank <- rep(NA, length(fix_these))
    rank[grepl(" ",x)] <- "species"
    rank[grepl(" \\w+ ",x)] <- "subspecies" # anything with more than two words is less than species
    rank[grepl(" subsp[. ]",x)] <- "subspecies"
    rank[grepl(" var[. ]",x)] <- "variety"
    rank[x == total$family.new[fix_these]] <- "family"
    rank[is.na(rank)] <- "genus" # single word and not family? genus.
    total$taxon.rank[fix_these] <- rank

    # get species and genus?
    total <- get_species_and_genus(total)

    total
}
