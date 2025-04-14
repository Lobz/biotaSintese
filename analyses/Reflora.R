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
write.csv(reflora, "data/derived-data/reflora.csv")
reflora <- read.csv("data/derived-data/reflora.csv")

setdiff(user_colnames, names(reflora))

# Apply plantR
occs <- formatDwc(user_data = reflora)
occs <- formatOcc(occs)
occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
txlist <- readRDS("data/derived-data/raw_dic_taxonomists.rds")
occs <- validateTax(occs, taxonomist.list = txlist) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?

write.csv(occs, "occs_reflora_post_plantR.csv")
