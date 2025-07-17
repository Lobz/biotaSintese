
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

plotMun <- function(name, plot = TRUE, save = TRUE) {
    gps_filter <- points_muns[[name]]
    filtered_gps <- valid_points[gps_filter,]
    # othermuns <- unique(filtered_gps$NAME_2)
    # plot(filtered_gps, add=T, col = "blue")
    filtered_name <- saopaulo[which(tolower(saopaulo$NAME_2)==tolower(name)),]
    name_filter1 <- which(valid_points$recordID %in% filtered_name$recordID)
    name_filter2 <- which(tolower(valid_points$NAME_2)==tolower(name))

    # Summary from GPS
    total <- nrow(filtered_gps)
    correct <- length(intersect(name_filter, gps_filter))
    na <- sum(is.na(filtered_gps$NAME_2))
    wrong <- total - correct - na
    summ_gps <- c(total=total,correct=correct,wrong=wrong,not_av=na)

    # Summary from Name
    total <- nrow(filtered_name)
    wrong <- length(name_filter) - correct
    na <- total - correct - wrong
    summ_name <- c(total=total,correct=correct,wrong=wrong,not_av=na)

    # Plots
    if(plot) {
        par(mfrow=c(2,2))
        plot(sp$geom, main=name)
        plot(st_geometry(shapes[name,]), add=T)
        plot(filtered_name, col=rgb(0,0,1,0.1), pch = 4, add=T)
        barplot(summ_name[2:4], main = "Coords of points filtered by municipality")
        barplot(summ_gps[2:4], main="Municipality of points filtered by GPS")
        # Third summary I guess
        barplot(sort(table(filtered_gps$NAME_2), decreasing = TRUE)[2:4], main="Top three wrong municipalities")
        if(save) {
            savePlot(paste0("plots/", tolower(plantR::rmLatin(name)), ".png"))
        }
    }

    dplyr::bind_rows(summ_gps, summ_name)
}

plotMun("Ubatuba")
plotMun("Campinas")
plotMun("Valinhos")
plotMun("Santo André")
plotMun("Santos")
plotMun("São Carlos")
plotMun("Campos Do Jordão")
plotMun("São Paulo")
