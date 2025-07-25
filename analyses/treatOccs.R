devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)

# Flag for reruning all analysis
rerun <- TRUE

# Data from Catalogo
load("data/raw-data/catalogoCompleto.RData")

# Data from previous runs
done <- read.csv("results/summary_multilist.csv")

# Data about UCs from CNUC
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC", "Municípios.Abrangidos"))
ucs$Nome.da.UC <- standardize_uc_name(ucs$Nome.da.UC)

# Select for treating: one or more records
has_records <- done$NumRecords > 0

# Select units for which this treatement was not done
untreated <- is.na(done$NumTaxons)
if (rerun) {
    to_treat <- has_records
} else {
    to_treat <- has_records & untreated
}

# Apply selection
ucs <- subset(ucs, Nome.da.UC %in% done$Nome.da.UC[to_treat])

# Select a subset of UCs (for testing)
# ucs <- subset(ucs, !grepl("-",Municípios.Abrangidos))
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

# If using sample, I want to remove sample from done
done <- subset(done, !Nome.da.UC %in% ucs$Nome.da.UC)

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
    Nome_UC <- uc_data$Nome.da.UC
    print("Getting data for UC:")
    print(Nome_UC)
    nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))
    # fix a couple common misspellings
    Nome_UC <- sub("AREA", "ÁREA", Nome_UC, fixed = T)
    Nome_UC <- sub("PATRIMONIO", "PATRIMÔNIO", Nome_UC, fixed = T)
    # Generate string for regex grepl in locality data
    uc_string <- generate_uc_string(Nome_UC)

    load(file=paste0("data/derived-data/occs_",nome_file,".RData"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    total <- validateLoc(total)

    # Match taxonomy to taxonomy backbone
    total <- getTaxonId(total)

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
    rank[is.na(rank)] <- tolower(total$taxonRank)[fix_these][is.na(rank)]
    rank[is.na(rank)] <- "genus"
    total$taxon.rank[fix_these] <- rank

    # get species and genus?
    total <- get_species_and_genus(total)

    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

    # Create my own checklist from the data treated with plantR
    species <- subset(total, taxon.rank %in% c("species","subspecies","variety","form")) # todo: check if this is correct
    sp <- unique(species$species.new)
    gen <- unique(species$genus.new)
    genus <- subset(total, !genus.new %in% gen)
    fam <- unique(c(species$family.new, genus$family.new))
    family <- subset(total, !family.new %in% fam)
    final <- dplyr::bind_rows(species, genus, family)

    # Get best records for each taxon
    top <- top_records(final, n = 1)
    write.csv(top, paste0("results/allfields/",nome_file,".csv"), na="", row.names=FALSE)

    # Remove unmatched?
    top <- subset(top, !is.na(id))

    print(paste("Found",nrow(top),"taxons."))
    ucs[i,]$NumTaxons <- nrow(top)
    ucs[i,]$NumSpecies <- length(unique(top$species.new))
    ucs[i,]$NumGenus <- length(unique(top$genus.new))
    ucs[i,]$NumFamilies <- length(unique(top$family.new))
    ucs[i,]$NumOuro <- sum(top$tax.check == "high")
    ucs[i,]$NumPrata <- sum(top$tax.check == "medium")
    ucs[i,]$NumBronze <- sum(top$tax.check == "low")
    ucs[i,]$NumLatao <- sum(top$tax.check == "unkown")

    # Get info from  F&FBR
    bf <- load_florabr(data_dir = "data/raw-data")
    ids <- substr(top$id, 5, nchar(top$id))
    matches <- match(ids, bf$id)
    unmatched <- which(is.na(ids))
    # Try with original name (catches some errors)
    matches[unmatched] <- match(top$scientificName[unmatched], bf$scientificName)
    # Try with species names (helps with errors with authorship, eliminates subspecies though)
    unmatched <- which(is.na(matches) & !is.na(top$species.new))
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

    write.csv(finalList, paste0("results/checklist/",nome_file,"_modeloCatalogo.csv"), na="", row.names=FALSE)
})
}

# Save summary
total <- dplyr::bind_rows(done, ucs)
total <- total[order(total$Nome.da.UC),]
write.csv(total, "results/summary_multilist.csv", row.names=FALSE)
summary(total==0)
summary(total<20)
