
get_species_and_genus <- function(x) {
    x$species.new <- NA
    x$species.new[which(x$taxon.rank=="species")] <- x$scientificName.new[which(x$taxon.rank=="species")]
    x$species.new[which(x$taxon.rank=="variety")] <- sub(" var.*$","",x$scientificName.new[which(x$taxon.rank=="variety")])
    x$species.new[which(x$taxon.rank=="subspecies")] <- sub(" subsp.*$","",x$scientificName.new[which(x$taxon.rank=="subspecies")])

    x$genus.new <- sub(" .*$","",x$scientificName.new)
    x$genus.new[x$taxon.rank == "family"] <- NA
    x
}
