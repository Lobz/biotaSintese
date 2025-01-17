
devtools::load_all()

library("plantR")

data(example_intro)
occs_splink <- example_intro

head(occs_splink)

familia <- "Blechnaceae"
splinkkey <- 'eZOGLZyihOoCLlAWs3Tx'
occs_splink <- rspeciesLink(family = familia, key = splinkkey)
str(occs_splink)
occs_gbif <- rgbif2(species = familia,
    country = "BR",
    stateProvince = "São Paulo",
    n.records = 450000)
dim(occs_splink)
table(is.na(occs_splink$locality))
dim(occs_gbif)
# Check GBIF docs - what's the diff between locality and verbatimLocality
table(is.na(occs_gbif$locality), is.na(occs_gbif$verbatimLocality))
# And how is it possible to have only one of them ???
subset(occs_gbif, is.na(locality) &! is.na(verbatimLocality))$verbatimLocality
unique(subset(occs_gbif, !is.na(locality) & is.na(verbatimLocality))$locality)
t(subset(occs_gbif, !is.na(locality) &! is.na(verbatimLocality))
    [,c("verbatimLocality","locality")])

head(occs_gbif)
occs <- formatDwc(gbif_data = occs_gbif, splink_data = occs_splink)
table(is.na(occs$locality))
sort(names(occs))
table(is.na(occs$locality), is.na(occs$loc))
samp <- sample(1:nrow(occs), 12)
occs[samp,"locality"]

occs <- formatOcc(occs)
names(occs)
occs <- formatLoc(occs)
t(occs[samp,c("locality", "locality.new", "stateProvince", "municipality", "loc")])

occs[1:10, 167:187]


# read data from file
gbif_raw <- readData("./data/0071227-241126133413365.zip")
str(gbif_raw)

occs <- formatDwc(gbif_data = gbif_raw[[1]], splink_data = occs_splink)
str(occs)
