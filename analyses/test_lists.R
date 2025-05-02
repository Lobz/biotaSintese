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
familiesRef <- unique(cl0$family.new)
length(familiesRef)
speciesRef <- unique(cl0$scientificName.new)
length(speciesRef)
str(cl0)
cl0 <- get_species_and_genus(cl0)


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

# Merge and treat data
occs <- formatDwc(
    splink_data = splink_raw
    )
occs <- formatOcc(occs)
occs <- formatLoc(occs)

# join with reflora and gbif
load("data/derived-data/reflora_gbif_jabot_saopaulo.RData")
occs <- dplyr::bind_rows(occs, saopaulo)

# occs <- formatCoord(occs)
# occs <- validateLoc(occs)

# Filter occs in the selected CU
avare <- subset(occs, municipality.new == "avare")
dim(avare)
avare1 <- subset(avare, grepl("horto florestal", locality.new, ignore.case = TRUE, perl = TRUE))
avare2 <- subset(occs, grepl(uc_string, locality.new, ignore.case = TRUE, perl = TRUE))
table(avare2$locality.new)
table(avare2$municipality)
table(avare1$locality.new)
dim(avare2)
dim(avare1) # 24
avare3 <- merge(avare1, avare2, all=T)
dim(avare3) # 371

# drop empty cols
avare3 <- avare3[,sapply(avare3, function(x) !all(is.na(x)))]
dim(avare3)

avare3$scientificNameAuthorship[is.na(avare3$scientificNameAuthorship)] <-
avare3$scientificNameAuthorship.x[is.na(avare3$scientificNameAuthorship)]

avare3 <- formatCoord(avare3)
avare3 <- formatTax(avare3)
avare3 <- validateLoc(avare3)
avare3 <- validateCoord(avare3) # resourse intensive - optimize?
avare3 <- validateTax(avare3, generalist = T)
avare3$tax.check <- factor(avare3$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)


# fix missing taxon rank
table((avare3$taxon.rank))
table(is.na(avare3$taxon.rank))
avare3 <- avare3[order(avare3$taxon.rank),]
fix_these <- which(is.na(avare3$taxon.rank))
x<- avare3$scientificName.new[fix_these]
avare3$taxon.rank[fix_these] <- avare3$taxon.rank[match(x, avare3$scientificName.new)]
fix_these <- which(is.na(avare3$taxon.rank))
x<- avare3$scientificName.new[fix_these]
x <- sub(" sp.","",x)
rank <- rep(NA,length(fix_these))
rank[grepl(" ",x)] <- "species"
rank[grepl("subsp",x)] <- "subspecies"
rank[x == avare3$family.new[fix_these]] <- "family"
rank[is.na(rank)] <- "genus"
avare3$taxon.rank[fix_these] <- rank

# get species and genus?
avare3 <- get_species_and_genus(avare3)

avare3 <- avare3[order(avare3$taxon.rank, avare3$tax.check, avare3$scientificName.new, as.numeric(avare3$year.new), as.numeric(avare3$yearIdentified.new), na.last=F, decreasing = T),]

save(avare3, file="data/derived-data/occs_avare.RData")
load("data/derived-data/occs_avare.RData")

avare4 <- validateDup(avare3, comb.fields = list(c("family", "col.last.name", "col.number")) ) # this removes dups? shouldn't we do this before other checks?
avare4[!is.na(avare4$dup.ID),]

table(avare3$scientificName.new, avare3$tax.check)
table(avare3$scientificName.new, avare3$taxon.rank, useNA = "always")

summ <- summaryData(avare3)

table(avare3$basisOfRecord, useNA="always")

# Try to create my own checklist from the data treated with plantR
species <- subset(avare3, taxon.rank %in% c("species","subspecies","variety"))
    sp <- unique(species$species.new)
    gen <- unique(species$genus.new)
genus <- subset(avare3, !genus.new %in% gen)
length(unique(avare3$genus.new[avare3$taxon.rank=="genus"]))
length(unique(genus$genus.new))
final <- rbind(species, genus)

compareLists <- function(l1, l2 = cl0) {
    sp1 <- unique(l1$species.new)
    sp2 <- unique(l2$species.new)
    gen1 <- unique(l1$genus.new)
    gen2 <- unique(l2$genus.new)
    f1 <- unique(l1$family.new)
    f2 <- unique(l2$family.new)

    print(c("List 1", sum(!is.na(sp1)), length(f1)))
    print(c("List 2", sum(!is.na(sp2)), length(f2)))

    print("SPECIES")
    print(c(length(setdiff(sp1, sp2)), length(setdiff(sp2,sp1))))
    print("FAMILIES")
    print(c(length(setdiff(f1, f2)), length(setdiff(f2,f1))))
}

compareLists(species)
compareLists(final)

compareLists(subset(final, tax.check >= "low"))
compareLists(subset(final, tax.check >= "medium"))
compareLists(subset(final, tax.check >= "high"))

table(species$downloadedFrom)

# Read list from GBIF species list tool
cl2 <- readData("data/gbif_estecolavare_specieslist.zip", quote = "", na.strings = c("", "NA"))
cl2 <- read.gbif("plantR_input/data/gbif_estecolavare_specieslist/0002783-250127130748423.csv")
dim(cl2) # 125 species
occs <- formatDwc(gbif_data = cl2)
occs <- formatTax(occs)
compareLists(subset(occs, taxon.rank=="species"))

table(lista$tax.check)


# Generate output file
finalList <- create_list(final, UC_de_interesse)
write.csv(finalList, "data/derived-data/checklist_avare_modeloCatalogo.csv")
