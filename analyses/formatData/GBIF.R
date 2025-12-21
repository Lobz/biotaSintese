devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_files <- list.files("data-input/GBIF", pattern = "*.zip", full.names = TRUE)
print("Reading gbif files:")
print(gbif_files)
gbif_data_raw <- lapply(gbif_files, readData, quote = "", na.strings = c("", "NA"))
gbif <- gbif_data_raw[[1]]
if(length(gbif_data_raw) > 1) {
    print("Merging GBIF databases...")
    for(i in 2:length(gbif_data_raw)){
        gbif$occurrence <- merge(gbif$occurrence, gbif_data_raw[[i]]$occurrence, all=T)
        gbif$citations <- merge(gbif$citations, gbif_data_raw[[i]]$citations, all=T)
    }
}
print(paste("Found",nrow(gbif$occurrence), "observations and", nrow(gbif$citations), "citations."))

write.csv(gbif$citations, "data-tmp/gbif-citations.csv")
gbif <- gbif$occurrence

gbif$taxonRank <- as.taxon.rank(tolower(gbif$taxonRank))
gbif$verbatimBasisOfRecord <- gbif$basisOfRecord
gbif$basisOfRecord <- as.basisOfRecord(gbif$basisOfRecord)

gbif$downloadedFrom <- "GBIF"

save(gbif, file="data-tmp/gbif_saopaulo_raw.RData")
