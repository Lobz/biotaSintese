devtools::load_all()
# splink data
splink_files <- list.files("data-input/Occurrences/splink", pattern = "*.txt$", full.names = TRUE)
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

splink <- formatSpLink(splink)
save(splink, file="data-tmp/splink.RData")
# todo: decide what to do with barcode NA