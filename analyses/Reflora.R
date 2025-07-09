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

# fix year data
reflora$year <- sub("^.*/","", reflora$year)
year <- reflora$year
correct <- nchar(year)==4
incomplete <- nchar(year)==2
date <- nchar(year)==6

year[date] <- getYear(as.Date(year[date], "%d%m%y"))
year[incomplete] <- ifelse(as.integer(year[incomplete]) > 25, paste0("19", year[incomplete]), year[incomplete])
reflora$year <- as.numeric(year)

reflora$month <- as.numeric(reflora$month)


save(reflora,file="data/raw-data/reflora_all.RData")
