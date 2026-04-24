devtools::load_all()
library(plantR)
# library(parallel)

print("Loading data...")
load("data-tmp/reflora_gbif_jabot_splink_x.RData")
# Treat gps data
print("Formatting coords...")
saopaulo <- formatCoord(saopaulo)

# formatTax and validateTax
print("Formatting taxonomy...")
saopaulo <- getTaxonId(saopaulo)

# We'll try getting extra taxons with wfo
# loading the WFO and WCVP backbones into a temporary environment
data(list = c("wfoNames", "wcvpNames"), package = "plantRdata")
# using the World Flora Online
# saopaulo <- tryAgain(saopaulo, not_found, getTaxonId, db = wfoNames)
# using the World Checklist of Vascular Plants
saopaulo <- tryAgain(saopaulo, not_found, getTaxonId, db = wcvpNames)

# Save unmatched taxons
nf <- saopaulo[saopaulo$tax.notes == "not found" | !startsWith(saopaulo$id, "bfo"), ]
nf <- aggregate(nf$catalogNumber, list(family=nf$family, scientificName=nf$scientificName, scientificNameAuthorship=nf$scientificNameAuthorship, id=nf$id), function(x) length(unique(x)))
nf <- nf[order(nf$family, nf$scientificName),]
write.csv(nf[nf$x>=10,], "results/taxons_not_found.csv", row.names=F)

# validate
print("Validating location info...")
saopaulo <- validateLoc(saopaulo)

print("Validating identification info...")
# validate taxonomist
saopaulo <- validateTax(saopaulo, generalist = T)
saopaulo$tax.check <- factor(saopaulo$tax.check, levels = c("unknown", "low", "medium", "high"), ordered = T)


print("Validating geolocation info...")
map <- latamMap$brazil
map <- subset(map, NAME_1 == "sao paulo")
saopaulo <- validateCoord(saopaulo, high.map = map) # WORKING
saopaulo <- tryAgain(saopaulo, function(x) is.na(x$decimalLatitude.new), formatCoord)
saopaulo <- tryAgain(saopaulo, function(x) is.na(x$geo.check), validateCoord, high.map=map)
tab(is.na(saopaulo$geo.check))
table(saopaulo$geo.check, saopaulo$origin.coord)

# substitute bad coords
# good_coords <- startsWith(saopaulo$geo.check, "ok_county") | startsWith(saopaulo$geo.check, "ok_locality")
# tab(good_coords)
# saopaulo$decimalLatitude.new[!good_coords] <- saopaulo$latitude.gazetteer[!good_coords]
# saopaulo$decimalLongitude.new[!good_coords] <- saopaulo$longitude.gazetteer[!good_coords]
# saopaulo$origin.coord[!good_coords] <- "coords_gazet"


saopaulo$recordID <- 1:nrow(saopaulo) # I need a unique ID for this

print("Saving...")
save(saopaulo,file="data-tmp/reflora_gbif_jabot_splink_saopaulo.RData")
