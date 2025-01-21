library(sf) # used for spatial operations

UC_de_interesse <- "Campos do Jordão"

ucs <- read.csv("~/BIOTA/unidades-de-conservacao/cnuc_2024_10.csv", sep=";", dec=",")
str(ucs)

table(ucs$Grupo)
table(ucs$Plano.de.Manejo)
table(ucs$UF)
ucs$in_SP <- grepl("SP",ucs$UF)

ucs_sp <- subset(ucs, in_SP)
table(ucs_sp$Grupo)
table(ucs_sp$Plano.de.Manejo)
sapply(ucs_sp, table)

ucs_sp[,c(2,4)]
subset(ucs, grepl(UC_de_interesse, Nome.da.UC, ignore.case=T))

# get shapes
shapes <- st_read("~/BIOTA/unidades-de-conservacao/shp/shp_cnuc_2024_10_pol.shp")
str(shapes)
my_UC <- subset(shapes, grepl(UC_de_interesse, nome_uc, ignore.case=T))
plot(my_UC[c(3,1,2),"nome_uc"])

# read data from file
gbif_raw <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
str(gbif_raw)
gbif_raw <- gbif_raw[[1]]
table(is.na(gbif_raw$locality), is.na(gbif_raw$verbatimLocality))
# Seems to me like we should use verbatimLocality whenever locality is missing
gbif_raw$locality[is.na(occs$locality)] <- gbif_raw$verbatimLocality[is.na(occs$locality)]

occs <- formatDwc(gbif_data = gbif_raw, drop = TRUE, drop.opt = TRUE, drop.empty = TRUE)
str(occs)
summary(is.na(occs$locality))
