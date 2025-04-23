library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()

# The point of this is to create and compare lists from different techniques
# For this exercise, we are using data about Estação Ecológica de Avaré

# Read list from Catalogo das Plantas das UCs do Brasil
cl0 <- read.csv(("data/raw-data/Dados_Catalogo_UCs_Brasil_EEAVARE.csv"))
names(cl0)
cl0$scientificName <- substr(cl0$Táxon, nchar(cl0$Família) + 2, nchar(cl0$Táxon))
cl0 <- formatTax(cl0)
dim(cl0)
str(cl0)

# Create list using plantR, GBIF, Reflora, splink
UC_de_interesse <- "ESTAÇÃO ECOLÓGICA DE AVARÉ"
uc_name <- prepLoc(UC_de_interesse)
uc_string <- "e(st(a[çc?][aã?]o|)?)?.? e(col([óo?]gica)?)?.?( de)? avar[ée?]"
county <- "Avaré"

# Splink data
splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
splink_raw <- rspeciesLink(stateProvince = "Sao Paulo", county = county, key = splinkkey, save = TRUE, dir = "data/", filename = "splink_county", MaxRecords = 2000)
splink_raw <- read.csv("data/splink_county.csv")
dim(splink_raw)
splink_raw$downloadedFrom <- "SPLINK"
sum(grepl(uc_string, splink_raw$locality, ignore.case = T, perl = T))

# Jabot data
jabot_raw <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot_raw$county <- NA
jabot_raw$downloadedFrom <- "JABOT"
sum(grepl(uc_string, jabot_raw$locality, ignore.case = T, perl = T))
# Jabot stores



# Merge and treat data
occs <- formatDwc(
    splink_data = splink_raw
    , user_data = jabot_raw
    )
occs <- formatOcc(occs)
occs <- formatLoc(occs)

# Filter occs in Sao Paulo
occs <- subset(occs, grepl("sao paulo", stateProvince.new))
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
occs <- validateTax(occs)
dim(occs)
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?
dim(occs)

# join with reflora and gbif
reflora <- read.csv("data/derived-data/occs_reflora_post_plantR.csv")
reflora <- subset(reflora, grepl("sao paulo", stateProvince.new))
gbif <- read.csv("data/derived-data/occs_gbif_post_plantR.csv")
gbif <- formatTax(gbif)
gbif <- validateTax(gbif)
gbif <- subset(gbif, grepl("sao paulo", stateProvince.new))

reflora <- lapply(reflora, as.character)
occs <- dplyr::bind_rows(occs, gbif)
occs <- dplyr::bind_rows(occs, reflora)
write.csv(occs, "data/derived-data/occs_sp_all_post_plantR.csv")
occs <- read.csv("data/derived-data/occs_sp_all_post_plantR.csv")
save(occs, file = "data/derived-data/occs_all_post_plantR.Rdata")
load("data/derived-data/occs_all_post_plantR.Rdata")

occs <- validateDup(as.data.frame(occs)) # this removes dups? shouldn't we do this before other checks?

# Filter occs in the selected CU
avare <- subset(occs, municipality.new == "avare")
dim(avare)
avare1 <- subset(avare, grepl("horto florestal", locality, ignore.case = TRUE, perl = TRUE))
avare2 <- subset(occs, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
table(avare2$locality.new)
table(avare2$municipality)
table(avare1$locality.new)
dim(avare2)
dim(avare1) # 176 records
avare3 <- merge(avare1, avare2, all=T)
dim(avare3)

# drop empty cols
avare3 <- avare3[,sapply(avare3, function(x) !all(is.na(x)))]

write.csv(avare3, "data/derived-data/occs_avare.csv")
avare3 <- read.csv("data/derived-data/occs_avare.csv")

avare3 <- formatCoord(avare3)
avare3 <- formatTax(avare3)
avare3 <- validateLoc(avare3)
avare3 <- validateCoord(avare3) # resourse intensive - optimize?
avare3 <- validateTax(avare3, generalist = T)
avare3 <- validateDup(avare3) # this removes dups? shouldn't we do this before other checks?

avare3$tax.check <- factor(avare3$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)
table(avare3$scientificName.new, avare3$tax.check)

summ <- summaryData(avare3)

cl1 <- checkList(avare3)
dim(cl1) # 129 different species??
table(cl1$family)

# Didn't like this result. Try to create my own checklist from the data treated with plantR
avaref <- avare3[, c("family.new", "scientificName.new", "year.new", "yearIdentified.new", "institutionCode", "collectionCode.new", "barcode", "identifiedBy.new", "tax.check", "taxon.rank", "downloadedFrom", "numTombo")]
species <- subset(avaref, taxon.rank == "species")

table(ouro$scientificName.new)

# Get most recent specimen/identification combo
splitdf <- by(species, species$scientificName.new, function(x) {
    x[order(x$tax.check, as.numeric(x$year.new), as.numeric(x$yearIdentified.new), na.last=F, decreasing = T),]
}, simplify = F)
species <- do.call(rbind, splitdf)
table(species$tax.check)

setdiff(cl0$scientificName.new, species$scientificName.new)
setdiff(subset(species, tax.check >= "high")$scientificName.new, cl0$scientificName.new)

# Read list created with CatalogoUCsBr

# Read list from GBIF species list tool
cl2 <- readData("data/gbif_estecolavare_specieslist.zip", quote = "", na.strings = c("", "NA"))
cl2 <- read.gbif("plantR_input/data/gbif_estecolavare_specieslist/0002783-250127130748423.csv")
dim(cl2) # 125 species
cl2 <- formatTax(cl2)
intersect(cl2$scientificName, avare1$scientificName)
setdiff(cl2$scientificName.new, cl1$scientificName.new)
setdiff(cl1$scientificName.new, cl2$scientificName.new)
