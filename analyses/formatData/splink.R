devtools::load_all()
# splink data
splink_files <- list.files("data-input/splink", pattern = "*.txt$", full.names = TRUE)
print("Reading splink files:")
print(splink_files)
splink_data_raw <- lapply(splink_files, read.csv, sep="\t", na.strings = c("", "NA"), quote = "")
splink <- splink_data_raw[[1]]
if(length(splink_data_raw) > 1) {
    print("Merging splink databases...")
    for(i in 2:length(splink_data_raw)){
        splink <- merge(splink, splink_data_raw[[i]], all=T)
    }
}

print(paste("Found",nrow(splink), "observations."))

# Normalize taxonRank and basisOfRecord
table(splink$basisofrecord, useNA="always")
splink$verbatimbasisofrecord <- splink$basisofrecord
splink$basisofrecord <- sub("([a-z])([A-Z])", "\\1_\\2", splink$basisofrecord)
splink$basisofrecord[which(startsWith(splink$basisofrecord, "Machine"))] <- "MACHINE_OBSERVATION"
splink$basisofrecord[which(startsWith(splink$basisofrecord, "Preserved"))] <- "PRESERVED_SPECIMEN"
splink$basisofrecord[which(startsWith(splink$basisofrecord, "Xil"))] <- "PRESERVED_SPECIMEN"
splink$basisofrecord[splink$basisofrecord=="Carpo"] <- "PRESERVED_SPECIMEN"
splink$basisofrecord <- toupper(splink$basisofrecord)
splink$basisofrecord <- as.basisOfRecord(splink$basisofrecord)

splink$recordedBy <- splink$collector
splink$recordNumber <- splink$fieldnumber
splink$scientificNameAuthorship <- splink$scientificnameauthor
save(splink, file="data-tmp/splink.RData")
# todo: decide what to do with barcode NA