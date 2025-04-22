library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()

# Splink data
splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
splink_raw <- rspeciesLink(stateProvince = "Sao Paulo", county = "Avaré", key = splinkkey, MaxRecords = 2000)
splink_raw$downloadedFrom <- "SPLINK"
dim(splink_raw)

# Jabot data
jabot_raw <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot_raw$county <- NA
jabot_raw$downloadedFrom <- "JABOT"
jabot_raw <- subset(jabot_raw, municipality == "Avaré" )

# Merge
occs <- formatDwc(
    splink_data = splink_raw
    , user_data = jabot_raw
    , drop.empty = T
    )
occs <- formatOcc(occs)
occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
occs <- validateTax(occs)
dim(occs)

# check results of different options
dups.default <- validateDup(occs)
dim(dups.default)
head(dups.default)
dups.remove <- validateDup(occs, remove = T)
dim(dups.remove)
dups.overwrite <- validateDup(occs, overwrite = T)
dim(dups.overwrite)
dups.removeoverwrite <- validateDup(occs, remove = T, overwrite = T)
dim(dups.removeoverwrite)

dups <- subset(dups.default, scientificName.new != scientificName.new1)
head(dups)

exID <- "[RB_1444837|SPSF_55309]"
subset(dups.default, dup.ID == exID)
subset(dups.remove, dup.ID == exID)
subset(dups.overwrite, dup.ID == exID)
subset(dups.removeoverwrite, dup.ID == exID)

dim(occs)