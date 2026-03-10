devtools::load_all()

# Reflora data
reflora_files <- list.files("data-input/Occurrences/REFLORA", pattern = "*.csv", full.names = TRUE)
print("Reading reflora files:")
print(reflora_files)
reflora_data_raw <- lapply(reflora_files, data.table::fread)
print("Parsing reflora data...")
reflora_data_parsed <- lapply(reflora_data_raw, parseReflora)
reflora <- reflora_data_parsed[[1]]
if(length(reflora_data_raw) > 1) {
    print("Merging reflora databases...")
    for(i in 2:length(reflora_data_raw)){
        reflora <- merge(reflora, reflora_data_parsed[[i]], all=T)
    }
}

print(paste("Found",nrow(reflora), "observations."))
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

reflora$basisOfRecord <- "PRESERVED_SPECIMEN"
reflora$basisOfRecord <- as.basisOfRecord(reflora$basisOfRecord)

save(reflora,file="data-tmp/reflora.RData")
