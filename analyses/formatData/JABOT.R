devtools::load_all()
# Jabot data
jabot_files <- list.files("data-input/Occurrences/JABOT", pattern = "*.csv", full.names = TRUE)
jabot_data_raw <- lapply(jabot_files, read.csv, sep="|", na.strings = c("", "NA"))
jabot <- jabot_data_raw[[1]]
if(length(jabot_data_raw) > 1) {
    print("Merging JABOT databases...")
    for(i in 2:length(jabot_data_raw)){
        # fix name typo
        names(jabot_data_raw[[i]])[names(jabot_data_raw[[i]])=="scientifcname"] <- "scientificName"
        #merge
        jabot <- merge(jabot, jabot_data_raw[[i]], all=T)
    }
}

print(paste("Found",nrow(jabot), "observations."))

jabot$county <- NA

# Normalize taxon Rank
jabot$verbatimTaxonRank <- jabot$taxonRank
jabot$taxonRank[grepl(" form.",jabot$scientificName)] <- "form"
jabot$taxonRank <- normalizeTaxonRank(jabot$taxonRank)

# Normalize basisOfRecord
if("basisofrecord" %in% names(jabot)) {
    jabot$verbatimBasisOfRecord <- jabot$basisofrecord
    jabot$basisofrecord[jabot$basisofrecord=="Preserved Specimen"] <- "PRESERVED_SPECIMEN"
    jabot$basisofrecord[jabot$basisofrecord=="Xiloteca"] <- "PRESERVED_SPECIMEN"
    jabot$basisofrecord <- as.basisOfRecord(jabot$verbatimBasisOfRecord)
} else {
    jabot$basisOfRecord <- NA
}

save(jabot,file="data-tmp/jabot.RData")
