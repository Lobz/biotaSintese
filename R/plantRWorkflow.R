
plantRWorkflow <- function(x) {
    x
# Standardize missing information
saopaulo[saopaulo==""] <- NA
# Subset country
print(dim(saopaulo))
saopaulo <- subset(saopaulo, is.na(country) | grepl("br", tolower(country), fixed=T))

# Standardize missing information
saopaulo[saopaulo==""] <- NA

# Select fields
f <-  plantR:::fieldNames
plantR_fields <- f[!is.na(f$type),c("plantr")]
extra_mine <- c("taxonRank", "verbatimScientificName", "acceptedScientificName", "species", "taxonID", "typeStatus", "recordID", "eventDate", "verbatimEventDate", "geodeticDatum", "associatedMedia",  "virtualDuplicates", "duplicates", "barcode", "downloadedFrom")
desired_fields <- union(plantR_fields, extra_mine)

saopaulo <- saopaulo[, intersect(desired_fields, names(saopaulo))]
print(dim(saopaulo))

# Lets format this
print("Formatting occs")
saopaulo <- formatOcc(saopaulo, noNumb = NA, noYear = NA, noName = NA)

print("Formatting locs")
saopaulo <- formatLoc(saopaulo)

# ###### PAUSE
print("Saving")
save(saopaulo, file="data-tmp/saopaulo_occs.RData")
save(saopaulo, file="data-tmp/reflora_gbif_jabot_splink_saopaulo.RData")

}