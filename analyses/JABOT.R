# Jabot data
jabot <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot$county <- NA

# Normalize taxon Rank
jabot$verbatimTaxonRank <- jabot$taxonRank
jabot$taxonRank[jabot$taxonRank=="f."] <- NA # these are unreliable, can be form, genus or family
jabot$taxonRank[grepl(" form.",jabot$scientificName)] <- "f."
jabot$taxonRank[jabot$taxonRank=="Infr."] <- NA # these are unreliable, and I couldn't figure out what this is supposed to mean
jabot$taxonRank <- factor(jabot$taxonRank,
    levels = c("f.", "var.", "subsp.", "sp.", "gen.", "fam.",
                "o", "c", "p", "k"), # these don't actually exist
    labels = taxonRanks,
    ordered = TRUE)

# Normalize basisOfRecord
table(jabot$basisofrecord, useNA="always")
jabot$verbatimBasisOfRecord <- jabot$basisofrecord
jabot$basisofrecord[jabot$basisofrecord=="Preserved Specimen"] <- "PRESERVED_SPECIMEN"
jabot$basisofrecord[jabot$basisofrecord=="Xiloteca"] <- "PRESERVED_SPECIMEN"
jabot$basisofrecord <- as.basisOfRecord(jabot$verbatimBasisOfRecord)

save(jabot,file="data/derived-data/jabot_saopaulo.RData")
