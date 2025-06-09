library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
devtools::load_all()

# Pre-treated data from GBIF, REflora and JABOT
load("data/derived-data/reflora_gbif_jabot_saopaulo.RData")

# Data from Catalogo
load("data/raw-data/catalogoCompleto.RData")

# Data from previous runs
done <- read.csv("data/derived-data/summary_multilist.csv")
done <- na.exclude(done)

# Data about UCs from CNUC
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC", "Municípios.Abrangidos"))

# Remove done
ucs <- subset(ucs, !Nome.da.UC %in% done$Nome.da.UC)

# Select a subset of UCs (for testing)
ucs <- subset(ucs, !grepl("-",Municípios.Abrangidos))
ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

# Make a summary table
ucs$NumRecords <- NA
ucs$NumTaxons <- NA
ucs$NumSpecies <- NA
ucs$NumGenus <- NA
ucs$NumFamilies <- NA

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
    county <- str_squish(gsub("\\(.*\\)","",uc_data$Municípios.Abrangidos))
    # Splink search has some issues with special characters so we look for both options
    county_splink <- paste(county, rmLatin(county))
    # This is to help look for municipality in plantR's municipality.new field
    county_plantr <- tolower(rmLatin(county))

    # Splink data
    splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
    splink_raw <- rspeciesLink(
        Scope = "p", # this should filter out animals, but unreliable in filtering out fungi, bacteria, etc
        stateProvince = "Sao Paulo", county = county_splink,
        key = splinkkey,
        MaxRecords = 5000)
    splink_raw <- subset(splink_raw, kingdom == "Plantae")
    splink_raw$downloadedFrom <- "SPLINK"

    # Merge and treat data
    occs <- formatDwc(
        splink_data = splink_raw
        )
    occs <- formatOcc(occs)
    occs <- formatLoc(occs)

    # join with jabot, reflora and gbif
    occs <- dplyr::bind_rows(occs, saopaulo)

    # this looks like a good place to force garbage collection
    gc()

    # Filter occs in the selected CU
    # Records in the municipality and in locality by type of CU
    occs_mun <- subset(occs, municipality.new == county_plantr)
    # parque <- subset(occs_mun, grepl("parque", locality.new, ignore.case = TRUE, perl = TRUE))
    # parque <- subset(parque,!grepl("parque estadual da vassununga", locality.new, perl = TRUE)) # todo: generalize this
    occs_uc_name <- subset(occs, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
    # if(grepl("PARQUE",Nome_UC)) {
        # total <- merge(occs_uc_name, parque, all=T)
    # } else {
        total <- occs_uc_name
    # }

    # TODO get records based on gps??

    if(nrow(total) == 0) {
        print("No records found for CU:")
        print(Nome_UC)

        ucs[i,3:7] <- 0

        next
    }

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    total <- validateLoc(total)

    # Fix some issues with taxonomy:

    # Some records don't have scientificName for some reason
    noName <- is.na(total$scientificName)
    table(noName)
    # if species is present, use that
    total$scientificName[noName] <- total$species[noName]
    noName <- is.na(total$scientificName)
    table(noName)
    # else, use genus
    total$scientificName[noName] <- total$genus[noName]
    noName <- is.na(total$scientificName)
    table(noName)
    # last resort, use family
    total$scientificName[noName] <- total$family[noName]
    noName <- is.na(total$scientificName)
    table(noName)

    # Match scientificName to oficial F&FBR backbone
    total <- formatTax(total)

    # we're gonna try again without author (see issue #170 in plantR)
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, use.author = F)

    # What's still unmatched? Genus rank
    total <- tryAgain(total, condition = function(x) x$tax.notes == "not found"& x$taxonRank=="GENUS", FUN = formatTax, tax.name = "genus")

    # What's still unmatched? Vars and subspecies
    total <- tryAgain(total,
        condition = function(x) x$tax.notes == "not found" & grepl("\\w+ \\w+ \\w", x$scientificName),
        FUN = function(x) {
            saved <- x$scientificName
            x$scientificName <- sub("(\\w+ \\w+ )", "\\1 var. ", x$scientificName)
            x <- formatTax(x)
            x$scientificName <- saved
            x
        })

    # What's still unmatched? Try again with less rigor?
    total <- tryAgain(total, function(x) x$tax.notes == "not found", formatTax, sug.dist=0.8 )


    # Finished; validate taxonomist
    total <- validateTax(total, generalist = T)
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)

    # fix missing taxon rank
    table((total$taxon.rank), useNA = "always")
    table(is.na(total$taxon.rank))
    total <- total[order(total$taxon.rank),]
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    # If there's another entry of the same taxon, get the taxon rank fron there
    total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    x <- sub(" sp.","",x)
    rank <- rep(NA,length(fix_these))
    rank[grepl(" ",x)] <- "species"
    rank[grepl(" subsp[. ]",x)] <- "subspecies"
    rank[grepl(" var[. ]",x)] <- "subspecies"
    rank[x == total$family.new[fix_these]] <- "family"
    rank[is.na(rank)] <- tolower(total$taxonRank)
    rank[is.na(rank)] <- "genus"
    total$taxon.rank[fix_these] <- rank

    # get species and genus?
    total <- get_species_and_genus(total)

    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

    save(total, file=paste0("data/derived-data/occs_",nome_file,".RData"))
    # load(file=paste0("data/derived-data/occs_",nome_file,".RData"))

    # Create my own checklist from the data treated with plantR
    species <- subset(total, taxon.rank %in% c("species","subspecies","variety","form")) # todo: check if this is correct
    sp <- unique(species$species.new)
    gen <- unique(species$genus.new)
    genus <- subset(total, !genus.new %in% gen) #TODO: add similar for family-level ids
    final <- rbind(species, genus)

    # Get best records for each taxon
    top <- top_records(final, n = 1)

    print(paste("Found",nrow(top),"taxons."))
    ucs[i,]$NumTaxons <- nrow(top)
    ucs[i,]$NumSpecies <- length(unique(top$species.new))
    ucs[i,]$NumGenus <- length(unique(top$genus.new))
    ucs[i,]$NumFamilies <- length(unique(top$family.new))

    write.csv(top, paste0("results/checklist_",nome_file,".csv"), na="")

    # Get info from  F&FBR
    bf <- load_florabr(data_dir = "data/raw-data")
    ids <- substr(top$id, 5, nchar(top$id))
    matches <- match(ids, bf$id)
    unmatched <- which(is.na(ids))
    # Try with original name (catches some errors)
    matches[unmatched] <- match(top$scientificName[unmatched], bf$scientificName)
    unmatched <- which(is.na(matches))
    # Try with species names (helps with errors with authorship, eliminates subspecies though)
    matches[unmatched] <- match(top$species.new[unmatched], bf$species)
    unmatched <- which(is.na(matches))

    # Extract origin and group information
    top$origin <-bf$origin[matches]
    # Group is probably dependent on family innit
    matches[unmatched] <- match(top$family.new[unmatched], bf$family)
    top$group <-bf$group[matches]

    # Generate output file
    finalList <- format_list(top, Nome_UC)

    # Add info about being new to catalogo
    UC_catalogo <- subset(catalogoCompleto, grepl(Nome_UC, Unidade.Conservação, perl = T, ignore.case = T))
    speciesCatalogo <- unique(UC_catalogo$scientificNameFull)
    listed <- finalList$Táxon_completo %in% UC_catalogo$Táxon
    finalList[,"Já listada"] <- ifelse(listed, "Sim", "Não")

    write.csv(finalList, paste0("results/checklist_",nome_file,"_modeloCatalogo.csv"), na="")
})
}

# Save summary
ucs <- dplyr::bind_rows(done, ucs)
write.csv(ucs, "results/summary_multilist.csv")
