devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_raw_gps <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
gbif_raw_texto <- readData("../../BIOTA/GBIF/0061630-241126133413365.zip", quote = "", na.strings = c("", "NA"))
dim(gbif_raw_gps$occurrence)
dim(gbif_raw_texto$occurrence)
gbif_raw <- merge(gbif_raw_gps$occurrence, gbif_raw_texto$occurrence, all=T)
dim(gbif_raw)
gbif_raw$downloadedFrom <- "GBIF"

write.csv(gbif_raw, "data/gbif_saopaulo.csv")

# Apply plantR
occs <- formatDwc(gbif_data = gbif_raw)
occs <- formatOcc(occs)
occs <- formatLoc(occs)

save(occs, file="data/derived-data/occs_gbif_saopaulo.RData")
