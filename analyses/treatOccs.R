devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)

# Data from previous runs
done <- read.csv("results/summary_multilist.csv")

# Select for treating: one or more records
has_records <- done$NumRecords > 0

# Apply selection
ucs <- done[has_records, ]

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

# If using sample, I want to remove sample from done
done <- subset(done, !Nome.da.UC %in% ucs$Nome.da.UC)
ucs$nome_file <- slug(ucs$Nome.da.UC)

# Space for summary
ucs$NumTaxons <- NA
ucs$NumSpecies <- NA
ucs$NumGenus <- NA
ucs$NumFamilies <- NA
ucs$NumOuro <- NA
ucs$NumPrata <- NA
ucs$NumBronze <- NA
ucs$NumLatao <- NA
ucs$NumNoMatch <- NA

# Load information for brazilian flora
# bf <- get_florabr(output_dir = "data-tmp")
bf <- load_florabr(data_dir = "data-tmp")

for(i in 1:sample_size){
try({

    uc_data <- ucs[i,]
    Nome_UC <- uc_data$Nome.da.UC
    print("Getting data for UC:")
    print(Nome_UC)
    nome_file <- uc_data$nome_file

    load(file=paste0("results/total/",nome_file,".rda"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)

    # Order occs
    total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]
# using the World Flora Online
total <- getTaxonId(total, complete = F, db = temp.env$wfoNames)
# using the World Checklist of Vascular Plants
total <- getTaxonId(total, complete = F, db = temp.env$wcvpNames)

    write.csv(total, paste0("results/total-treated/",nome_file,".csv"),  na="", row.names=FALSE)

    # # Detail locality quality
    # gps_orig <- total$selectionCategory == "coord_orig"
    # total$confidenceLocality[gps_orig] <- "None"
    # good_coords <- startsWith(total$geo.check, "ok_county") | startsWith(total$geo.check, "ok_locality")
    # unsure_coords <- total$geo.check %in% c("sea", "shore")
    # total$confidenceLocality[gps_orig & good_coords] <- "Medium" #todo: evaluate quality of gps polygon
    # total$confidenceLocality[gps_orig & unsure_coords] <- "Low" #todo: evaluate quality of gps polygon
    # total$confidenceLocality <- factor(total$confidenceLocality, levels = c("None", "Low", "Medium", "High"), ordered = T)

    # Avoid taxons that are already represented by more detailed taxons
    total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)
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
    # top <- subset(top, !unmatched)

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
    ids <- substr(top$id, 5, nchar(top$id))
    matches <- match(ids, bf$id)

    # Extract origin and group information
    top$origin <-bf$origin[matches]
    top$group <-bf$group[matches]

    # Generate output file
    finalList <- format_list(top, Nome_UC)

    write.csv(finalList, paste0("results/checklist/",nome_file,"_modeloCatalogo.csv"), na="", row.names=FALSE)
})
}
ucs$nome_file <- NULL

# Save summary
total <- dplyr::bind_rows(done, ucs)
total <- total[order(total$Nome.da.UC),]
write.csv(total, "results/summary_treatOccs.csv", row.names=FALSE)
summary(total==0)
summary(total<20)
