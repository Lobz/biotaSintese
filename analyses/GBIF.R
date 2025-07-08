devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_raw_gps <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
gbif_raw_texto <- readData("../../BIOTA/GBIF/0061630-241126133413365.zip", quote = "", na.strings = c("", "NA"))
dim(gbif_raw_gps$occurrence)
dim(gbif_raw_texto$occurrence)
gbif <- merge(gbif_raw_gps$occurrence, gbif_raw_texto$occurrence, all=T)
dim(gbif)

gbif$taxonRank <- factor(tolower(gbif$taxonRank), levels = taxonRanks, ordered= TRUE)

gbif$downloadedFrom <- "GBIF"

save(gbif, file="data/raw-data/gbif_saopaulo_raw.RData")
