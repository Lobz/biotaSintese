devtools::load_all()
library(plantR)

load("data-tmp/gbif.RData")
gbif$downloadedFrom <- "GBIF"
goodNames <- names(gbif)
gbif <- remove_fields(gbif)
load("data-tmp/reflora.RData")
reflora$downloadedFrom <- "Reflora"
reflora <- consolidateCase(reflora, goodNames)
goodNames <- union(goodNames, names(reflora))
reflora <- remove_fields(reflora)
load("data-tmp/jabot.RData")
jabot$downloadedFrom <- "JABOT"
jabot <- consolidateCase(jabot, goodNames)
goodNames <- union(goodNames, names(jabot))
jabot <- remove_fields(jabot)
load("data-tmp/splink.RData")
splink$downloadedFrom <- "Splink"
splink <- consolidateCase(splink, goodNames)
splink <- remove_fields(splink)

# Join all this together
saopaulo1 <- formatDwc(gbif_data = gbif, user_data = jabot)
saopaulo2 <- formatDwc(splink_data = splink, user_data = reflora)
saopaulo <- dplyr::bind_rows(saopaulo1, saopaulo2)

rm(gbif, reflora, jabot, splink, goodNames, saopaulo1, saopaulo2)

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