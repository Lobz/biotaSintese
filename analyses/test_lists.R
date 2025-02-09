library(plantR) # used foi reading and cleaning occurrence data
# GBIF checklist
gbif_raw <- readData("./data/gbif_estecolavare_specieslist.zip", quote = "", na.strings = c("", "NA"))
