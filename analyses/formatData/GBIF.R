devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_files <- list.files("data-input/Occurrences/GBIF", pattern = "*.zip", full.names = TRUE)
print("Reading gbif files:")
print(gbif_files)
gbif_data_raw <- lapply(gbif_files, readGBIF)
gbif <- gbif_data_raw[[1]]
if(length(gbif_data_raw) > 1) {
    print("Merging GBIF databases...")
    for(i in 2:length(gbif_data_raw)){
        gbif<- merge(gbif, gbif_data_raw[[i]], all=T)
    }
}
print(paste("Found",nrow(gbif), "observations"))

gbif <- formatGBIF(gbif)

save(gbif, file="data-tmp/gbif.RData")
