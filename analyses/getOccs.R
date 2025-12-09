devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
library(parallel)
library(sf)

# Data about UCs from CNUC
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC"))

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
ucs$NumNoMatch <- NA

# Standardize names and reorder
ucs$Nome.da.UC <- standardize_uc_name(ucs$Nome.da.UC)
ucs <- ucs[order(ucs$Nome.da.UC), ]

# Lookup what are the names of UCs in plantR
LT <- read.csv("results/locations/uc_locstrings.csv")
LT[LT==""] <- NA
loc1 <- aggregate(LT$loc.correct, list(Nome_UC = LT$uc_name), function(x) paste(x, collapse="|"))
LT <- na.omit(LT)
loc2 <- aggregate(LT$loc.extra, list(Nome_UC = LT$uc_name), function(x) paste(unique(x), collapse="|"))
LT <- rbind(loc1, loc2)
tail(LT)
loc3 <- aggregate(LT$x, list(Nome_UC = LT$Nome_UC), function(x) paste(x, collapse="|"))
rownames(loc3) <- loc3$Nome_UC

# Pre-treated data from GBIF, REflora and JABOT
load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

# Which occs are associated with each UC
occs_exact <- lapply(ucs$Nome.da.UC, function(s) {
    if(!s %in% loc3$Nome_UC) return(FALSE)
    grepl(loc3[s, "x"], saopaulo$loc.correct, perl=T)
})

# Read table of alternative names and locality names
checkedLocations <- read.csv("results/locations/checkedLocations.csv")
checkedLocations$Nome_UC <- toupper(standardize_uc_name(checkedLocations$Nome_UC))

# add oficial names
officialNames <- data.frame(Nome_UC = standardize_uc_name(ucs$Nome.da.UC), Municipio="QUALQUER", Localidade = ucs$Nome.da.UC, Relação = "Igual", Confiança = "Ouro")
LT <- rbind(checkedLocations, officialNames)

# Summarize alternative names
LT <- aggregate(LT$Localidade, list(Nome_UC = LT$Nome_UC, Municipio = LT$Municipio, relationship = LT$Relação, confidenceLocality = LT$Confiança), function(x) paste(unique(x), collapse="|"))
LT$Locality <- toupper(LT$x)
LT$x <- NULL

# Temporary
LT <- subset(LT, Municipio == "QUALQUER")
LT <- LT[ LT$Nome_UC %in% checkedLocations$Nome_UC,]
ucs <- ucs[ ucs$Nome.da.UC %in% checkedLocations$Nome_UC, ]

# Generate string for regex grepl in locality data
LT$uc_strings <- generate_uc_string(LT$Locality)
# Use regex to look for more occs
occs_loc <- lapply(LT$uc_strings, grepl, x = paste(saopaulo$municipality, saopaulo$locality), ignore.case = TRU, perl = TRUE)
occs_ucs <- pairwiseMap(occs_exact, occs_loc, FUN=function(x,y) {x|y})
names(occs_ucs) <- LT$Nome_UC
save(occs_ucs, file="data/derived-data/occs_ucs.RData")

ucs$loc.correct <- NULL

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
(sample_size = nrow(ucs))

# Data with valid coordinates: either original coordinates or locality
valid_coords <- subset(saopaulo, origin.coord == "coord_original" | resolution.gazetteer == "locality")
str(valid_coords)
valid_points <- st_as_sf(valid_coords, coords = c("decimalLongitude.new", "decimalLatitude.new"))
# Unify and convert datum to match SIRGAS 2000
valid_points <- fixDatum(valid_points)

# Shape data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)
shapes <- subset(shapes, nome_uc %in% ucs$Nome.da.UC)
shapes <- shapes[order(shapes$nome_uc), ]

# Intersect points with shapes
points_ucs <- st_intersects(shapes, valid_points)
names(points_ucs) <- shapes$nome_uc
sapply(points_ucs, length)
save(points_ucs, file="data/derived-data/points_ucs.RData")
# load("data/derived-data/points_ucs.RData")

# Get intersection table
intersecUCs <- read.csv("results/locations/intersecUCs.csv")
# Attribute confidence based on intersections
intersecUCs$confidence <- ifelse(intersecUCs$prop > 98, "High",
                             ifelse(intersecUCs$status == "covered_buffer" | intersecUCs$prop > 80, "Medium", "Low"))
intersecUCs$nome_uc <- standardize_uc_name(intersecUCs$nome_uc)
intersecUCs$outra_uc <- standardize_uc_name(intersecUCs$outra_uc)

intersecUCs <- subset(intersecUCs, outra_uc %in% ucs$Nome.da.UC)

ucs$nome_file <- slug(ucs$Nome.da.UC)

for(i in 1:sample_size){
try({

    uc_data <- ucs[i,]
    print("Getting data for UC:")
    print(uc_data[1])
    Nome_UC <- uc_data$Nome.da.UC
    nome_file <- uc_data$nome_file

    # Which records are in the gps shp
    rcs_intersect <- valid_points$recordID[points_ucs[[Nome_UC]]]
    occs_gps <- saopaulo$recordID %in% rcs_intersect

    # Generate string for regex grepl in locality data
    intersected <- subset(intersecUCs, nome_uc == Nome_UC)
    high <- intersected$outra_uc[intersected$confidence=="High"]
    medium <- intersected$outra_uc[intersected$confidence=="Medium"]

    occs_high <- Reduce('|', occs_ucs[high])
    occs_medium <- Reduce('|', occs_ucs[medium])

    if(length(high)==0){
        occs_high <- FALSE
    }
    if(length(medium)==0){
        occs_medium <- FALSE
    }


    # Exact UC name
    occs_uc_name <- occs_ucs[[Nome_UC]]
    occs_plantr <- occs_loc[[Nome_UC]]
    occs_string <- occs_exact[[Nome_UC]]

    # Join all filters
    occs_total <- occs_uc_name | occs_gps | occs_high | occs_medium
    if(!any(occs_total)) {
        print("No records found for CU:")
        print(Nome_UC)

        ucs[i,2:ncol(ucs)] <- 0

        next
    }

    # What quality is the locality
    saopaulo$confidenceLocality <- "Low" # GPS data
    saopaulo$confidenceLocality[occs_medium] <- "Medium"
    saopaulo$confidenceLocality[occs_uc_name | occs_high] <- "High"

    # What criteria was used to select each record
    saopaulo$selectionCategory <- saopaulo$origin.coord
    saopaulo$selectionCategory[occs_medium] <-  "locality_medium"
    saopaulo$selectionCategory[occs_high] <- "locality_high"
    saopaulo$selectionCategory[occs_uc_name] <- "locality_exact"
    saopaulo$selectionCategory[occs_plantr] <- "plantr_exact"

    total <- saopaulo[occs_total,]

    total$Nome_UC <- Nome_UC
    save(total, file=paste0("results/total/",nome_file,".rda"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)
})
}

ucs$nome_file <- NULL

# Save summary
write.csv(ucs, "results/summary_multilist.csv", row.names=FALSE)
summary(ucs==0)
summary(ucs<20)
