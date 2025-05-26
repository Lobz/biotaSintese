library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()

# The point of this is to create and compare lists from different techniques
# For this exercise, we are using data about Estação Ecológica de Avaré
ucs <- read.csv("~/BIOTA/unidades-de-conservacao/cnuc_2024_10.csv", sep=";", dec=",")

# Create list using plantR, GBIF, Reflora, splink
UC_de_interesse <- "ESTAÇÃO ECOLÓGICA DE AVARÉ"
uc_string <- "e(st(a[çc?][aã?]o|)?)?.? e(col([óo?]gica)?)?.?( de)? avar[ée?]"

# UC_de_interesse <- "PORTO FERREIRA"
uc_data <- subset(ucs, grepl(UC_de_interesse, Nome.da.UC, ignore.case=T))
Nome_UC <- uc_data$Nome.da.UC
nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))
# uc_string <- generate_uc_string(Nome_UC)
county <- str_squish(gsub("\\(.*\\)","",uc_data$Municípios.Abrangidos))
county_splink <- paste(county, rmLatin(county))
county_plantr <- tolower(rmLatin(county))

# Splink data
splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
splink_raw <- rspeciesLink(
    scope = "p",
    stateProvince = "Sao Paulo", county = county_splink,
    key = splinkkey,
    save = TRUE, dir = "data/", filename = "splink_county", MaxRecords = 2000)
splink_raw <- read.csv("data/splink_county.csv")
dim(splink_raw)
table(splink_raw$county)
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

occs[occs==""] <- NA

# occs <- formatCoord(occs)
# occs <- validateLoc(occs)

# Filter occs in the selected CU
occs_mun <- subset(occs, municipality.new == county_plantr)
dim(occs_mun)
horto <- subset(occs_mun, grepl("horto florestal", locality.new, ignore.case = TRUE, perl = TRUE))
table(grepl(county,ucs$Municípios.Abrangidos))
parque <- subset(occs_mun, grepl("parque", locality.new, ignore.case = TRUE, perl = TRUE))
parque <- subset(parque,!grepl("parque estadual da vassununga", locality.new, perl = TRUE)) # todo: generalize this
occs_uc_name <- subset(occs, grepl(uc_string, locality.new, ignore.case = TRUE, perl = TRUE))
table(parque$locality.new)
table(parque$municipality.new)
table(occs_uc_name$locality.new)
table(occs_uc_name$municipality.new)
dim(horto)
dim(parque)
dim(occs_uc_name) # 24
if(grepl("PARQUE",Nome_UC)) {
    total <- merge(occs_uc_name, parque, all=T)
} else {
    total <- merge(occs_uc_name, horto, all=T)
}
dim(total) # 371

# drop empty cols
total <- total[,sapply(total, function(x) !all(is.na(x)))]
dim(total)

total <- formatCoord(total)
total <- formatTax(total)
total <- validateLoc(total)
total <- validateCoord(total) # resourse intensive - optimize?
total <- validateTax(total, generalist = T)
total$tax.check <- factor(total$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)


# fix missing taxon rank
table((total$taxon.rank), useNA = "always")
table(is.na(total$taxon.rank))
total <- total[order(total$taxon.rank),]
fix_these <- which(is.na(total$taxon.rank))
x<- total$scientificName.new[fix_these]
total$taxon.rank[fix_these] <- total$taxon.rank[match(x, total$scientificName.new)]
fix_these <- which(is.na(total$taxon.rank))
x<- total$scientificName.new[fix_these]
x <- sub(" sp.","",x)
rank <- rep(NA,length(fix_these))
rank[grepl(" ",x)] <- "species"
rank[grepl(" subsp[. ]",x)] <- "subspecies"
rank[grepl(" var[. ]",x)] <- "subspecies"
rank[x == total$family.new[fix_these]] <- "family"
rank[is.na(rank)] <- "genus"
total$taxon.rank[fix_these] <- rank

# get species and genus?
total <- get_species_and_genus(total)

total <- total[order(total$taxon.rank, total$tax.check, total$scientificName.new, as.numeric(total$year.new), as.numeric(total$yearIdentified.new), na.last=F, decreasing = T),]

save(total, file=paste0("data/derived-data/occs_",nome_file,".RData"))

table(total$scientificName.new, total$tax.check)
table(total$scientificName.new, total$taxon.rank, useNA = "always")

summ <- summaryData(total)

table(total$basisOfRecord, useNA="always")

# Try to create my own checklist from the data treated with plantR
species <- subset(total, taxon.rank %in% c("species","subspecies","variety")) # todo: check if this is correct
    sp <- unique(species$species.new)
    gen <- unique(species$genus.new)
genus <- subset(total, !genus.new %in% gen) #TODO: add similar for family-level ids
length(unique(total$genus.new[total$taxon.rank=="genus"]))
length(unique(genus$genus.new))
final <- rbind(species, genus)

# load comparison data
load("data/raw-data/catalogoCompleto.RData")
UC_catalogo <- subset(catalogoCompleto, grepl(Nome_UC, Unidade.Conservação, perl = T, ignore.case = T))
dim(UC_catalogo)
speciesCatalogo <- unique(UC_catalogo$scientificNameFull)

compareLists <- function(l1, l2 = UC_catalogo) {
    sp1 <- unique(l1$species.new)
    sp2 <- unique(l2$species.new)
    gen1 <- unique(l1$genus.new)
    gen2 <- unique(l2$genus.new)
    f1 <- unique(l1$family.new)
    f2 <- unique(l2$family.new)

    print(c("List 1", sum(!is.na(sp1)), length(f1)))

    print("SPECIES")
    print(c(length(setdiff(sp1, sp2)), length(setdiff(sp2,sp1))))
    print("FAMILIES")
    print(c(length(setdiff(f1, f2)), length(setdiff(f2,f1))))
}

compareLists(UC_catalogo)
compareLists(final)

# compareLists(subset(final, tax.check >= "low"))
compareLists(subset(final, tax.check >= "medium"))
compareLists(subset(final, tax.check >= "high"))


# Generate output file
finalList <- create_list(final, Nome_UC)
listed <- finalList$Táxon_completo %in% UC_catalogo$scientificNameFull
finalList[,"Já listada"] <- ifelse(listed, "Sim", "Não")

table(finalList$Origem)
table(finalList$Origem[!listed])
table(finalList$Localidade[!listed])
table(is.na(finalList$Barcode))

write.csv(finalList, paste0("data/derived-data/checklist_",nome_file,"_modeloCatalogo.csv"), na="")
