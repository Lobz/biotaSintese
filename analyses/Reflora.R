devtools::load_all()

# minFields <- c("collectionCode", "catalogNumber", "recordNumber", "recordedBy", "year", "country", "stateProvince", "county", "municipality", "decimalLatitude", "decimalLongitude", "identifiedBy", "dateIdentified", "typeStatus", "scientificName", "scientificNameAuthorship", "institutionCode")

# user_colnames <- c("institutionCode", "collectionCode",
#                               "catalogNumber",
#                               "recordNumber", "recordedBy",
#                               "year", "country", "stateProvince", "county",
#                               "municipality", "locality",
#                               "decimalLatitude", "decimalLongitude",
#                               "identifiedBy", "dateIdentified",
#                               "typeStatus", "family", "scientificName",
#                               "scientificNameAuthorship")
# setdiff(user_colnames, minFields)
# d <- t(data.frame(minFields))
# names(d) <- minFields
# formatDwc(user_data=d) ## TODO open issue about this


# Reflora data
reflora_raw <- data.table::fread("../../BIOTA/REFLORA/REFLORA.csv")
names(reflora_raw)
reflora <- parseReflora(reflora_raw)
reflora$downloadedFrom <- "REFLORA"
reflora <- as.data.frame(reflora)

save(reflora,file="data/raw-data/reflora_all.RData")
