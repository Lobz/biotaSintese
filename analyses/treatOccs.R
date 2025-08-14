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
ucs <- done[to_treat, ]

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

# If using sample, I want to remove sample from done
done <- subset(done, !Nome.da.UC %in% ucs$Nome.da.UC)

for(i in 1:sample_size){
try({

    uc_data <- ucs[i,]
    Nome_UC <- uc_data$Nome.da.UC
    print("Getting data for UC:")
    print(Nome_UC)
    nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))

    load(file=paste0("results/total/occs_",nome_file,".RData"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    total <- validateLoc(total)

    # Match taxonomy to taxonomy backbone
    total <- getTaxonId(total)

    # Finished; validate taxonomist
    total <- validateTax(total, generalist = T)
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)

    # fix missing taxon rank
    total$taxon.rank <- as.taxon.rank(total$taxon.rank)
    table((total$taxon.rank), useNA = "always")
    table(is.na(total$taxon.rank))
    # If there's another entry of the same taxon, get the taxon rank from there
    total <- total[order(total$taxon.rank),]
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
    # If possible, get it from taxonRank
    fix_these <- which(is.na(total$taxon.rank))
    total$taxon.rank[fix_these] <- tolower(total$taxonRank[fix_these])
    # Otherwise, look at scientific name
    fix_these <- which(is.na(total$taxon.rank))
    x<- total$scientificName.new[fix_these]
    x <- sub(" sp.","",x)
    rank <- rep(NA, length(fix_these))
    rank[grepl(" ",x)] <- "species"
    rank[grepl(" \\w+ ",x)] <- "subspecies" # anything with more than two words is less than species
    rank[grepl(" subsp[. ]",x)] <- "subspecies"
    rank[grepl(" var[. ]",x)] <- "variety"
    rank[x == total$family.new[fix_these]] <- "family"
    rank[is.na(rank)] <- "genus" # single word and not family? genus.
    total$taxon.rank[fix_these] <- rank

    # get species and genus?
    total <- get_species_and_genus(total)

    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

    write.csv(total, paste0("results/total-treated/",nome_file,".csv"),  na="", row.names=FALSE)

    # Avoid taxons that are already represented by more detailed taxons
    subspecies <- subset(total, taxon.rank < "species")
    sp <- unique(subspecies$species.new)
    species <- subset(total, taxon.rank == "species" & !species.new %in% sp)
    gen <- unique(c(subspecies$genus.new, species$genus.new))
    genus <- subset(total, taxon.rank == "genus" & !genus.new %in% gen)
    fam <- unique(c(subspecies$family.new, species$family.new, genus$family.new))
    family <- subset(total, taxon.rank == "family" & !family.new %in% fam)

    final <- dplyr::bind_rows(subspecies, species, genus, family)

    # Get best records for each taxon
    top <- top_records(final, n = 1)
    write.csv(top, paste0("results/allfields/",nome_file,".csv"), na="", row.names=FALSE)

    # Remove unmatched?
    unmatched <- is.na(top$id)
    top <- subset(top, !unmatched)

    print(paste("Found",nrow(top),"taxons."))
    ucs[i,]$NumTaxons <- nrow(top)
    ucs[i,]$NumSpecies <- length(unique(top$species.new))
    ucs[i,]$NumGenus <- length(unique(top$genus.new))
    ucs[i,]$NumFamilies <- length(unique(top$family.new))
    ucs[i,]$NumOuro <- sum(top$tax.check == "high")
    ucs[i,]$NumPrata <- sum(top$tax.check == "medium")
    ucs[i,]$NumBronze <- sum(top$tax.check == "low")
    ucs[i,]$NumLatao <- sum(top$tax.check == "unknown")
    ucs[i,]$NumNoMatch <- sum(unmatched)

    # Get info from  F&FBR
    bf <- load_florabr(data_dir = "data/raw-data")
    ids <- substr(top$id, 5, nchar(top$id))
    matches <- match(ids, bf$id)

    # Extract origin and group information
    top$origin <-bf$origin[matches]
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
total$NumLatao <- total$NumTaxons - total$NumOuro - total$NumPrata - total$NumBronze
total$V11 <- NULL
write.csv(total, "results/summary_multilist.csv", row.names=FALSE)
summary(total==0)
summary(total<20)
