library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()

# The point of this is to create and compare lists from different techniques
# For this exercise, we are using data about Estação Ecológica de Avaré

# Read list from Catalogo das Plantas das UCs do Brasil
cl0 <- read.csv(("../../BIOTA/catalogoUCs/lista_CatUCS_avare.csv"))

# Create list using plantR, GBIF, Reflora, splink
UC_de_interesse <- "ESTAÇÃO ECOLÓGICA DE AVARÉ"
uc_name <- prepLoc(UC_de_interesse)
uc_string <- "e(st(a[çc?][aã?]o|)?)?.? e(col([óo?]gica)?)?.?( de)? avar[ée?]"
county <- "Avaré"

# Splink data
splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
splink_raw <- rspeciesLink(stateProvince = "Sao Paulo", county = county, key = splinkkey, save = TRUE, dir = "data/", filename = "splink_county", MaxRecords = 2000)
splink_raw <- data.table::fread("data/splink_county.csv")
dim(splink_raw)
splink_raw$downloadedFrom <- "SPLINK"
sum(grepl(uc_string, splink_raw$locality, ignore.case = T, perl = T))

# Jabot data
jabot_raw <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot_raw$county <- NA
jabot_raw$downloadedFrom <- "JABOT"
sum(grepl(uc_string, jabot_raw$locality, ignore.case = T, perl = T))
# Jabot stores

user.data <- dplyr::bind_rows(reflora_raw, jabot_raw)

# Merge and treat data
occs <- formatDwc(
    splink_data = splink_raw
    , user_data = user.data
    )
occs <- formatOcc(occs)
occs <- formatLoc(occs)
# Filter occs in Sao Paulo
occs <- subset(occs, grepl("sao paulo", stateProvince.new))

# taxonomist list from guilherme
txlist <- readRDS("data/derived-data/raw_dic_taxonomists.rds")

occs <- formatCoord(as.data.frame(occs))
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
occs <- validateTax(occs)
occs <- validateTax(occs, taxonomist.list = txlist) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?


# Filter occs in the selected CU
avare <- subset(occs, municipality.new == "avare")
dim(avare)
avare1 <- subset(avare, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
avare2 <- subset(occs, grepl(uc_string, locality, ignore.case = TRUE, perl = TRUE))
table(avare2$locality.new)
table(avare2$municipality)
table(avare1$locality.new)
dim(avare2)
dim(avare1) # 176 records

write.csv(avare2, "data/occs_avare.csv")

summ <- summaryData(occs)

cl1 <- checkList(avare2)
dim(cl1) # 124 different species??
table(cl1$family)
subset(cl1, family.new == "Fabaceae")

# Didn't like this result. Try to create my own checklist from the data treated with plantR

# Read list created with CatalogoUCsBr

# Read list from GBIF species list tool
cl2 <- readData("data/gbif_estecolavare_specieslist.zip", quote = "", na.strings = c("", "NA"))
cl2 <- read.gbif("plantR_input/data/gbif_estecolavare_specieslist/0002783-250127130748423.csv")
dim(cl2) # 125 species
cl2 <- formatTax(cl2)
intersect(cl2$scientificName, avare1$scientificName)
setdiff(cl2$scientificName.new, cl1$scientificName.new)
setdiff(cl1$scientificName.new, cl2$scientificName.new)
