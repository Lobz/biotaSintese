library(plantR) # used foi reading and cleaning occurrence data
devtools::load_all()

minFields <- c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year", "country", "stateProvince", "county", "municipality", "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")

user_colnames <- c("institutionCode", "collectionCode",
                              "catalogNumber",
                              "recordNumber", "recordedBy",
                              "year", "country", "stateProvince", "county",
                              "municipality", "locality",
                              "decimalLatitude", "decimalLongitude",
                              "identifiedBy", "dateIdentified",
                              "typeStatus", "family", "scientificName",
                              "scientificNameAuthorship")
setdiff(user_colnames, minFields)
d <- t(data.frame(minFields))
names(d) <- minFields
formatDwc(user_data=d) ## TODO open issue about this


# Reflora data
reflora_raw <- data.table::fread("../../BIOTA/REFLORA/REFLORA.csv")
names(reflora_raw)
reflora <- parseReflora(reflora_raw)
reflora$downloadedFrom <- "REFLORA"

setdiff(user_colnames, names(reflora))

# Apply plantR
occsR <- formatDwc(user_data = as.data.frame(reflora))
occsR <- formatOcc(occsR)
occsR <- formatLoc(occsR)

save(occsR,file="data/derived-data/occsR_reflora.RData")
