devtools::load_all()
library(sf) # used for spatial operations
library(plantR) # used foi reading and cleaning occurrence data

UC_de_interesse <- "JORDÃO"
# UC_de_interesse <- "PORTO FERREIRA"

ucs <- read.csv("data/raw-data/cnuc_2025_03.csv", sep=";", dec=",")
str(ucs)
ucs$Nome.da.UC <- standardize_uc_name(ucs$Nome.da.UC)
uc_data <- subset(ucs, grepl(UC_de_interesse, Nome.da.UC, ignore.case=T))
Nome_UC <- uc_data$Nome.da.UC
nome_file <- gsub(" ","",tolower(rmLatin(Nome_UC)))

ucs_icmbio <- read.csv("data/raw-data/DadosGeoestatisticos_UCs_21jul2025.csv", skip=4)
str(ucs_icmbio)
ucs_icmbio$Nome.da.UC <- standardize_uc_name(ucs_icmbio$Nome.da.Unidade.de.Conservação.conforme.o.Ato.legal)

dim(ucs)
dim(ucs_icmbio)
not_found <- setdiff(ucs_icmbio$Nome.da.UC, ucs$Nome.da.UC)
not_found <- sub("PROTEÇÃO AMBIENTAL D..? ", "PROTEÇÃO AMBIENTAL ", not_found)
(not_found <- setdiff(not_found, ucs$Nome.da.UC))
not_found <- sub("PROTEÇÃO AMBIENTAL ", "PROTEÇÃO AMBIENTAL DE ", not_found)
(not_found <- setdiff(rmLatin(not_found), rmLatin(ucs$Nome.da.UC)))
nomes <- rmLatin(ucs$Nome.da.UC)
nomes <- gsub(" D\\w\\w? ", " ", nomes)
not_found <- gsub(" D\\w\\w? ", " ", not_found)
(not_found <- setdiff(not_found, nomes))
nomes <- gsub(" - ", "-", nomes)
not_found <- gsub(" - ", "-", not_found)
(not_found <- setdiff(not_found, nomes))


table(ucs$Grupo)
table(ucs$Plano.de.Manejo)
table(ucs$UF)
ucs$in_SP <- grepl("SP",ucs$UF)

ucs_sp <- subset(ucs, in_SP)
table(ucs_sp$Grupo)
table(ucs_sp$Plano.de.Manejo)
sapply(ucs_sp, table)

sample(ucs_sp$Nome.da.UC,10)

ucs_sp[,c(2,4)]
subset(ucs, grepl(UC_de_interesse, Nome.da.UC, ignore.case=T))

# get shapes
shapes <- st_read("~/BIOTA/unidades-de-conservacao/shp/shp_cnuc_2024_10_pol.shp")
str(shapes)
shapes_sp <- subset(shapes, grepl("SÃO PAULO", uf))
my_UC <- subset(shapes, grepl(UC_de_interesse, nome_uc, ignore.case=T))
plot(my_UC[c(3,1,2),"nome_uc"])

# read data from file
load(paste0("data/derived-data/occs_",nome_file[4],".RData"))

str(total)
dim(total)
summary(is.na(total$locality))


# Create sf points for all records
my_points <- st_as_sf(total, coords = c("decimalLongitude.new", "decimalLatitude.new"))
plot(my_points[,"NAME_2"])
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
uctotal_gps <- my_points[lst[[1]],]
dim(uctotal_gps)
sort(table(uctotal_gps$locality.new))
plot(st_geometry(my_UC))
plot(uctotal_gps[,"municipality.new"], add=T)
savePlot("EtsEcolAvare.png")

locs <- unlist(strsplit(tolower(uctotal_gps$locality),",|;| - "))
locs <- gsub("[ .]$","",locs)
state <- unique(tolower(uctotal_gps$stateProvince))

table(locs)[table(locs)>15]
table(is.na(uctotal_gps$locality))

subset(uctotal_gps, is.na(locality))

# by locality
uc_string <- "esta[çc?][aã?]o ecol[óo?]gica de avar[ée?]"
uctotal_byloc <- subset(my_points, grepl(uc_string, locality, ignore.case = TRUE))
dim(uctotal_byloc)

plot(st_geometry(my_UC))
plot(uctotal_byloc[,1], col="blue", add=T)
savePlot("EtsEcolAvare_byloc.png")

uctotal <- uctotal_byloc

# Create checklist
sp1 <- unique(uctotal$scientificName.new1)
sp_gps <- unique(uctotal_gps$scientificName.new1)
setdiff(sp_gps, sp1)
setdiff(sp1, sp_gps)

summ <- summaryData(uctotal[1:50,])
list <- checkList(total,
            n.vouch=3, # max number of vouchers per species (hopefully it will order from best to worst?)
            type = "selected", # unsure what the options mean
            rm.dup = TRUE, # remove duplicates!! (does it unify duplicates??)
            rank.type = 5 # this controls voucher ranking I think?
            )
str(list)
save.image()
