devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(florabr)
library(parallel)
library(sf)

# Pre-treated data from GBIF, REflora and JABOT
load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")
saopaulo$recordID <- 1:nrow(saopaulo) # I need a unique ID for this

# Data with valid coordinates: either original coordinates or locality
valid_coords <- subset(saopaulo, origin.coord == "coord_original" | resolution.gazetteer == "locality")
valid_points <- st_as_sf(valid_coords, coords = c("decimalLongitude.new", "decimalLatitude.new"))
# Unify and convert datum to match SIRGAS 2000
valid_points <- fixDatum(valid_points)

# Data about UCs from CNUC
ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
ucs <- subset(ucs, grepl("SP|SAO PAULO", UF), select = c("Nome.da.UC"))
ucs$Nome.da.UC <- standardize_uc_name(ucs$Nome.da.UC)
ucs <- ucs[order(ucs$Nome.da.UC),]

# Generate string for regex grepl in locality data
uc_strings <- generate_uc_string(ucs$Nome.da.UC)

# Which occs are associated with each UC
occs_ucs <- lapply(uc_strings, grepl, x = saopaulo$locality, ignore.case = TRUE, perl = TRUE)
names(occs_ucs) <- ucs$Nome.da.UC

# Select a subset of UCs (for testing)
# ucs <- subset(ucs, !grepl("-",Municípios.Abrangidos))
# ucs <- ucs[sample(1:nrow(ucs), 10), ]
sample_size = nrow(ucs)

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

# Get intersection table
intersecUCs <- read.csv("results/intersecUCs.csv")
# Attribute confidence based on intersections
intersecUCs$confidence <- ifelse(intersecUCs$prop > 98, "High",
                             ifelse(intersecUCs$status == "covered_buffer" | intersecUCs$prop > 80, "Medium", "Low"))

for(i in 1:sample_size){
try({

    uc_data <- ucs[i,]
    print("Getting data for UC:")
    print(uc_data)
    Nome_UC <- uc_data$Nome.da.UC
    nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))

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

    # Join all filters
    occs_total <- occs_uc_name | occs_gps | occs_high | occs_medium
    if(!any(occs_total)) {
        print("No records found for CU:")
        print(Nome_UC)

        ucs[i,3:11] <- 0

        next
    }

    saopaulo$confidenceLocality <- ifelse(occs_uc_name | occs_high, "High", ifelse(occs_medium, "Medium", "Low"))
    total <- saopaulo[occs_total,]
    saopaulo$confidenceLocality <- NA

    # total$uc_name <- Nome_UC
    save(total, file=paste0("results/total/occs_",nome_file,".RData"))
    # load(file=paste0("data/derived-data/occs_",nome_file,".RData"))

    print(paste("Found",nrow(total),"records."))
    ucs[i,]$NumRecords <- nrow(total)
})
}

# Data from previous runs
done <- read.csv("results/summary_multilist.csv")
unchanged <- (done$NumRecords == ucs$NumRecords)
ucs[!unchanged,]<-done[unchanged,]

# Save summary
write.csv(ucs, "results/summary_multilist.csv", row.names=FALSE)
summary(ucs==0)
summary(ucs<20)
