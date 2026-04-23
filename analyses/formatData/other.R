
require(integraFlora)
require(plantR)

# other data
other_files <- list.files("data-input/Occurrences/OtherSources", pattern = "*.csv", full.names = TRUE)
if(length(other_files) > 0) {
    print("Reading other files:")
    print(other_files)
    other_data_raw <- lapply(other_files, readOccurrence)
    print("Parsing other data...")
    other <- lapply(other_data_raw, formatOccurrence)
} else {
    other <-list()
}

save(other,file="data-tmp/other.RData")