devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
library(parallel)
library(sf)

# Data about UCs from CNUC
print("Loading conservation units data...")
ucs <- read.csv("data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC"))

# Make a summary table
ucs$NumRecords <- NA

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
# load("data-tmp/reflora_gbif_jabot_splink_saopaulo.RData")
print("Loading occurrence data...")
load("data-tmp/reflora_gbif_jabot_splink_saopaulo_deduped.RData")

# Which occs are associated with each UC
occs_exact <- sapply(ucs$Nome.da.UC, function(s) {
    if(!s %in% loc3$Nome_UC) return(FALSE)
    grepl(loc3[s, "x"], sp_deduped$loc.correct, perl=T)
}, USE.NAMES = TRUE, simplify = FALSE)

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
LT <- subset(LT, confidenceLocality == "Ouro")

# Generate string for regex grepl in locality data
LT$uc_strings <- generate_uc_string(LT$Locality)
# Use regex to look for more occs
occs_loc_mun <- pairwiseMap(LT$uc_strings, LT$Municipio, function(str, mun) {
    if(mun=="QUALQUER") {
        res <- searchLoc(str, sp_deduped)
    } else {
        in_mun <- which(sp_deduped$municipality.correct == mun)
        res <- rep(FALSE, nrow(sp_deduped))
        res[in_mun] <- searchLoc(str, sp_deduped[in_mun, ])
    }
    res
}, simplify = FALSE)
names(occs_loc_mun) <- LT$Nome_UC
# Combine positive matches from different municipalities
occs_loc <- sapply(unique(LT$Nome_UC), function(n) {Reduce("|", occs_loc_mun[n])}, simplify = FALSE, USE.NAMES = TRUE)
# Combine matches from occs_exact and occs_loc
occs_ucs <- pairwiseMap(occs_exact[names(occs_loc)], occs_loc, FUN=function(x,y) {x|y})
names(occs_ucs) <- names(occs_loc)

# Remove loc.correct column
ucs$loc.correct <- NULL

# Select a subset of UCs (for testing)
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
(sample_size = nrow(ucs))

# Data with valid coordinates: either original coordinates or locality
print("Selecting and correcting valid georeferenced points (original coords) ...")
coords_original <- subset(sp_deduped, origin.coord == "coord_original" |  resolution.gazetteer == "locality")
coords_original <- st_as_sf(coords_original, coords = c("decimalLongitude.new", "decimalLatitude.new"))
coords_original <- fixDatum(coords_original) # Unify and convert datum to match SIRGAS 2000
print("Selecting and correcting valid georeferenced points (gazet coords) ...")
coords_gazet <- subset(sp_deduped, resolution.gazetteer == "locality")
coords_gazet <- st_as_sf(coords_gazet, coords = c("longitude.gazetteer", "latitude.gazetteer"))
st_crs(coords_gazet) <- "EPSG:4674" # Assumes datum is SIRGAS 2000 (used by IBGE)


# Shape data
print("Loading multipolygons...")
shapes <- st_read("data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)
shapes <- subset(shapes, nome_uc %in% ucs$Nome.da.UC)
shapes <- shapes[order(shapes$nome_uc), ]

# Intersect points with shapes
print("Intersecting points and shapes...")
points_ucs_original <- st_intersects(shapes, coords_original)
points_ucs_gazet <- st_intersects(shapes, coords_gazet)
names(points_ucs_original) <- names(points_ucs_gazet) <- shapes$nome_uc

# Get intersection table
print("Reading intersection table...")
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
    rcs_intersect <- coords_original$recordID[points_ucs_original[[Nome_UC]]]
    occs_gps_original <- sp_deduped$recordID %in% rcs_intersect
    rcs_intersect <- coords_gazet$recordID[points_ucs_gazet[[Nome_UC]]]
    occs_gps_gazet <- sp_deduped$recordID %in% rcs_intersect
    occs_gps_both <- occs_gps_original & occs_gps_gazet

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
    occs_total <- occs_uc_name | occs_gps_original | occs_gps_gazet | occs_high | occs_medium
    if(!any(occs_total)) {
        print("No records found for CU:")
        print(Nome_UC)

        ucs[i,2:ncol(ucs)] <- 0

        next
    }

    # What criteria was used to select each record
    sp_deduped$selectionCategory <- sp_deduped$origin.coord
    sp_deduped$selectionCategory[occs_gps_both] <-  "coords_both"
    sp_deduped$selectionCategory[occs_medium] <-  "locality_medium"
    sp_deduped$selectionCategory[occs_high] <- "locality_high"
    sp_deduped$selectionCategory[occs_uc_name] <- "locality_exact"
    sp_deduped$selectionCategory[occs_plantr] <- "plantr_exact"

    # What quality is the locality
    sp_deduped$confidenceLocality <- "Low" # original GPS data
    sp_deduped$confidenceLocality[occs_medium | occs_gps_gazet] <- "Medium"
    sp_deduped$confidenceLocality[occs_uc_name | occs_high | occs_gps_both] <- "High"

    total <- sp_deduped[occs_total,]

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
