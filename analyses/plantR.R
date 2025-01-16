splinkkey <- 'eZOGLZyihOoCLlAWs3Tx'

remotes::install_github("LimaRAF/plantR", ref = "dev")

library("plantR")

data(example_intro)
occs_splink <- example_intro

head(occs_splink)

familia <- "Blechnaceae"
occs_splink <- rspeciesLink(family = familia, key = splinkkey)
occs_gbif <- rgbif2(species = familia,
    country = "BR",
    n.records = 450000)
dim(occs_splink)

devtools::load_all()
gbif_raw <- readData("./data/0071227-241126133413365.zip")
str(gbif_raw)

occs <- formatDwc(gbif_data = gbif_raw[[1]], splink_data = occs_splink)
str(occs)
