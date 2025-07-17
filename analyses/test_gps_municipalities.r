
devtools::load_all()
library(stringr)
library(florabr)
library(parallel)
library(sf)
library(geobr)

# Pre-treated data from GBIF, REflora and JABOT
load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")
saopaulo$recordID <- 1:nrow(saopaulo) # I need a unique ID for this

# Data with valid coordinates: either original coordinates or locality
valid_coords <- subset(saopaulo, origin.coord == "coord_original" | resolution.gazetteer == "locality")
valid_points <- st_as_sf(valid_coords, coords = c("decimalLongitude.new", "decimalLatitude.new"))
# Unify and convert datum to match SIRGAS 2000
valid_points <- fixDatum(valid_points)

# Get shapes for municipalities
shapes <- read_municipality("SP")
rownames(shapes) <- shapes$name_muni

# Get shape for São Paulo
sp <- read_state("SP")

# Intersect points with shapes
points_muns <- st_intersects(shapes, valid_points)
names(points_muns) <- shapes$name_muni
sapply(points_muns, length)

plotMun <- function(name) {
    dev.off()
    gps_filter <- points_muns[[name]]
    filtered_gps <- valid_points[gps_filter,]
    # othermuns <- unique(filtered_gps$NAME_2)
    plot(sp$geom, main=name)
    plot(st_geometry(shapes[name,]), add=T)
    # plot(filtered_gps, add=T, col = "blue")
    name_filter <- which(valid_points$NAME_2==name)
    filtered_name <- valid_points[name_filter,]
    plot(filtered_name, col=rgb(0,0,1,0.1), pch = 4, add=T)

    # Summary from GPS
    total <- nrow(filtered_gps)
    correct <- sum(filtered_gpsname$NAME_2 == name, na.rm = T)
    wrong <- sum(filtered_gpsname$NAME_2 != name, na.rm = T)
    na <- sum(is.na(filtered_gpsname$NAME_2))
    summ_gps <- (round(100*c(correct, wrong, na)/total))
    # Summary from Name
    total <- nrow(filtered_name)
    correct <- sum(, na.rm = T)
    wrong <- sum(filtered_gpsname$NAME_2 != name, na.rm = T)
    na <- sum(is.na(filtered_gpsname$NAME_2))
    (round(100*c(correct, wrong, na)/total))
}

plotMun("Ubatuba")
plotMun("Campinas")
plotMun("Valinhos")
