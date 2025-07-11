devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)

# Pre-treated data from GBIF, REflora and JABOT
load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

# Data about UCs from CNUC
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC", "Municípios.Abrangidos"))

# Select a subset of UCs (for testing)
# ucs <- subset(ucs, !grepl("-",Municípios.Abrangidos))
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

# Make a summary table
ucs$NumRecords <- NA
ucs$NumTaxons <- NA
ucs$NumSpecies <- NA
ucs$NumGenus <- NA
ucs$NumFamilies <- NA
ucs$NumOuro <- NA
ucs$NumPrata <- NA
ucs$NumBronze <- NA
ucs$NumLatao <- NA

for(i in 1:sample_size){
try({

    uc_data <- ucs[i,]
    print("Getting data for UC:")
    print(uc_data)
    Nome_UC <- uc_data$Nome.da.UC
    nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))
    # fix a couple common misspellings
    Nome_UC <- sub("AREA", "ÁREA", Nome_UC, fixed = T)
    Nome_UC <- sub("PATRIMONIO", "PATRIMÔNIO", Nome_UC, fixed = T)
    # Generate string for regex grepl in locality data
    uc_string <- generate_uc_string(Nome_UC)
    # Get municípios
    # split municipios
    # Remove ones in other states (?), download each separately
    counties <- strsplit(uc_data$Municípios.Abrangidos, " - ")[[1]]
    counties <- counties[endsWith(counties,"(SP)")]
    counties <- substr(counties, 0, nchar(counties)-5)
    county <- paste(counties, collapse = " ")
    # This is to help look for municipality in plantR's municipality.new field
    county_plantr <- tolower(rmLatin(county))

    # Filter occs in the selected CU
    # Records in the municipality and in locality by type of CU
    # occs_mun <- subset(occs, municipality.new == county_plantr)
    # parque <- subset(occs_mun, grepl("parque", locality.new, ignore.case = TRUE, perl = TRUE))
    # parque <- subset(parque,!grepl("parque estadual da vassununga", locality.new, perl = TRUE)) # todo: generalize this
    occs_uc_name <- subset(saopaulo, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
    # if(grepl("PARQUE",Nome_UC)) {
        # total <- merge(occs_uc_name, parque, all=T)
    # } else {
        total <- occs_uc_name
    # }

    # TODO get records based on gps??

    if(nrow(total) == 0) {
        print("No records found for CU:")
        print(Nome_UC)

        ucs[i,3:11] <- 0

        next
    }

    save(total, file=paste0("data/derived-data/occs_",nome_file,".RData"))
    # load(file=paste0("data/derived-data/occs_",nome_file,".RData"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)
})
}

# Save summary
ucs <- ucs[order(ucs$Nome.da.UC),]
write.csv(ucs, "results/summary_multilist.csv", row.names=FALSE)
summary(ucs==0)
summary(ucs<20)
