devtools::load_all()
library(plantR) # used foi reading and cleaning occurrence data

# GBIF data
gbif_raw_gps <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
gbif_raw_texto <- readData("../../BIOTA/GBIF/0061630-241126133413365.zip", quote = "", na.strings = c("", "NA"))
dim(gbif_raw_gps$occurrence)
dim(gbif_raw_texto$occurrence)
gbif_raw <- merge(gbif_raw_gps$occurrence, gbif_raw_texto$occurrence, all=T)
dim(gbif_raw)
gbif_raw$downloadedFrom <- "GBIF"

write.csv(gbif_raw, "data/gbif_saopaulo.csv")
# gbif_raw <- data.table::fread("data/gbif_saopaulo.csv")
gbif_raw <- read.csv("data/gbif_saopaulo.csv")

# Apply plantR
occs <- formatDwc(gbif_data = gbif_raw)
occs <- formatOcc(occs)
occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
txlist <- readRDS("data/derived-data/raw_dic_taxonomists.rds")
occs <- validateTax(occs, taxonomist.list = txlist) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?

write.csv(occs, "occs_gbif_post_plantR.csv")
occs <- read.csv("occs_gbif_post_plantR.csv")

table(occs$tax.check1)

# Add specialists with lots of ids to taxonomist list?
str(txlist)
names(occs)
taxonomists <- unique(occs$identifiedBy.new)
families <- unique(occs$family.new)

getIdentificationDistribution <- function(data, idr) {
    s <- subset(data, identifiedBy.new == idr)
    table(s$family.new)
}

getSpecialization <- function(occs, idr, cutoff.flat = 200) {
    s <- subset(occs, identifiedBy.new == idr, select="family.new")
    t <- table(s$family.new)

    t <- t[t > cutoff.flat]
    names(t)
}

lapply(taxonomists, getSpecialization, occs=occs, cutoff.flat = 500)


table(subset(occs, identifiedBy.new == "Souza, V.C.")$family.new)
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?
