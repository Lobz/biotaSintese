library(plantR) # used foi reading and cleaning occurrence data
library(readxl)

# The point of this is to create and compare lists from different techniques
# For this exercise, we are using data about Estação Ecológica de Avaré

# Create list using plantR, GBIF, Reflora, splink
UC_de_interesse <- "ESTAÇÃO ECOLÓGICA DE AVARÉ"
uc_name <- prepLoc(UC_de_interesse)
uc_string <- "est(a[çc?][aã?]o|)?.? ecol([óo?]gica)?.?( de)? avar[ée?]"

# GBIF data
gbif_raw_gps <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
gbif_raw_texto <- readData("../../BIOTA/GBIF/0061630-241126133413365.zip", quote = "", na.strings = c("", "NA"))
dim(gbif_raw_gps$occurrence)
dim(gbif_raw_texto$occurrence)
gbif_raw <- merge(gbif_raw_gps$occurrence, gbif_raw_texto$occurrence, all=T)
dim(gbif_raw)
write.csv(gbif_raw, "data/gbif_saopaulo.csv")

sum(grepl(uc_string, gbif_raw$locality, ignore.case = T, perl = T))


# Splink data
splinkkey <- 'eZOGLZyihOoCLlAWs3Tx'
splink_raw <- rspeciesLink(stateProvince = "Sao Paulo", key = splinkkey)

sum(grepl(uc_string, splink_raw$locality, ignore.case = T, perl = T))
# Jabot data
jabot_raw <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|")
jabot_raw$county <- NA
sum(grepl(uc_string, jabot_raw$locality, ignore.case = T, perl = T))

# Reflora data
reflora_raw <- read_excel("../../BIOTA/REFLORA/REFLORA.xlsx")
names(reflora_raw)
sum(grepl(uc_string, data$locality, ignore.case = T, perl = T))

# Merge and treat data
occs <- formatDwc(
    gbif_data = gbif_raw,
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
occs <- validateTax(occs) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?

write.csv(occs, "data/occs_sp.csv")

# Filter occs in the selected CU
avare <- subset(occs, municipality.new == "avare")
dim(avare)
table(avare$locality)
avare <- subset(occs, grepl(uc_name, locality.new))
dim(avare)
avare <- subset(occs, grepl(uc_string, locality.new, ignore.case = TRUE, perl = TRUE))
dim(avare)

summ <- summaryData(occs)

list <- checkList(occs)
str(list)


# Read list created with CatalogoUCsBr

# Read list from GBIF species list tool
gbif_raw <- readData("./data/gbif_estecolavare_specieslist.zip", quote = "", na.strings = c("", "NA"))