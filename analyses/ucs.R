library(sf) # used for spatial operations
library(plantR) # used foi reading and cleaning occurrence data

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
shapes_sp <- subset(shapes, grepl("SÃO PAULO", uf))
my_UC <- subset(shapes, grepl(UC_de_interesse, nome_uc, ignore.case=T))
plot(my_UC[c(3,1,2),"nome_uc"])

# read data from file
gbif_raw <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip", quote = "", na.strings = c("", "NA"))
str(gbif_raw)
gbif_raw <- gbif_raw[[1]]
names(gbif_raw)
# where is datum?
table(gbif_raw$geodeticDatum)
table(gbif_raw$verbatimCoordinateSystem)

# verbatim Locality?
table(is.na(gbif_raw$locality), is.na(gbif_raw$verbatimLocality))
# Seems to me like we should use verbatimLocality whenever locality is missing
gbif_raw$locality[is.na(gbif_raw$locality)] <- gbif_raw$verbatimLocality[is.na(gbif_raw$locality)]

occs <- formatDwc(gbif_data = gbif_raw
    # , drop = TRUE
    # , drop.opt = TRUE
    # , drop.empty = TRUE
)
str(occs)
dim(occs)
summary(is.na(occs$locality))

# Clean data
# occs <- formatOcc(occs) # see issue 125 in plantR
# Copying from formatOccs
occs <- getCode(occs)
# Collector number
occs$recordNumber.new <- colNumber(occs$recordNumber, noNumb = "s.n.")

# Collection year
occs$year.new <- getYear(occs$year, noYear = "n.d.")

# Identifier name
occs$identifiedBy.new <- fixName(occs$identifiedBy)

# Identification year
occs$yearIdentified.new <- getYear(occs$dateIdentified, noYear = "n.d.")

## Putting people's names into the default name notation and
#separating main and auxiliary names
recordedBy.new <- fixName(occs$recordedBy)
# occs$recordedBy.aux <- prepName(recordedBy.new,
#                             fix.names = FALSE,
#                             sep.out = "; ",
#                             output = "aux")
occs$recordedBy.new <- prepName(recordedBy.new,
                            fix.names = FALSE,
                            output = "first")

occs$identifiedBy.aux <- prepName(occs$identifiedBy.new,
                                fix.names = FALSE,
                                sep.out = "; ",
                                output = "aux")
occs$identifiedBy.new <- prepName(occs$identifiedBy.new,
                                fix.names = FALSE,
                                output = "first")

## Standardize the notation for missing names
occs$recordedBy.new <- missName(occs$recordedBy.new,
                            type = "collector",
                            noName = "s.n.")
occs$identifiedBy.new <- missName(occs$identifiedBy.new,
                                type = "identificator",
                                noName = "s.n.")

## Extract the last name of the collector
occs$last.name <- lastName(occs$recordedBy.new,
                        noName = "s.n.")



occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
occs <- validateTax(occs) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?
summ <- summaryData(occs)

# Create sf points for all records
my_points <- st_as_sf(occs, coords = c("decimalLongitude.new", "decimalLatitude.new"))
plot(my_points[,"municipality"])
plot(shapes_sp[2], col="grey", add= TRUE) ## WHY NOT WORK????



# Select records occuring in each UC

# figure out datum
datum <- toupper(gbif_raw$geodeticDatum)
table(datum)
datum[grepl("84", datum)] <- "WGS84"
datum[grepl("4326", datum)] <- "EPSG:4326"
datum[grepl("SAD69", datum)] <- "EPSG:4618"
datum[grepl("ED50", datum)] <- "EPSG:4230"
datum[grepl("TWD67", datum)] <- "EPSG:3821"
datum[grepl("NOT|UNKNOWN|DESCONHECIDO", datum)] <- NA
datum[grepl("SIRGAS 200", datum)] <- "EPSG:4674"

table(datum)
crs <- sapply(datum, st_crs)
str(crs)

# for now, since vast majority is WGS84, I'll just use that
st_crs(my_points) <- "WGS84"
my_points <- st_transform(my_points, st_crs(my_UC))

# Now we should be able to match points
lst <- st_intersects(my_UC, my_points)
mat <- st_intersects(my_points, my_UC, sparse = FALSE)
str(lst)
# who falls within each UC
plot(st_geometry(my_UC))
plot(my_points[lst[[3]],], col="gray", add=T)
plot(my_points[lst[[1]],], col="red", add=T)
plot(my_points[lst[[2]],], col="blue", add=T)
str(my_points)

# now lets see what we can get from each UC
pecj <- my_points[lst[[1]],]
dim(pecj)
str(pecj)
sort(table(pecj$locality))

locs <- unlist(strsplit(tolower(pecj$locality),",|;| - "))
locs <- gsub("[ .]$","",locs)
state <- unique(tolower(pecj$stateProvince))

table(locs)[table(locs)>10]

# Create checklist
summ <- summaryData(occs)
list <- checkList(occs,
            n.vouch=3, # max number of vouchers per species (hopefully it will order from best to worst?)
            type = "selected", # unsure what the options mean
            rm.dup = TRUE, # remove duplicates!! (does it unify duplicates??)
            rank.type = 5 # this controls voucher ranking I think?
            )
str(list)
save.image()
