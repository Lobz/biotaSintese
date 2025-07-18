
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
    name_filter <- which(tolower(valid_points$NAME_2)==tolower(name))

    # Summary from GPS
    total <- nrow(filtered_gps)
    correct <- length(intersect(name_filter, gps_filter))
    na <- sum(is.na(filtered_gps$NAME_2))
    wrong <- total - correct - na
    summ_gps <- c(total_gps=total,correct=correct,wrong_name=wrong,name_not_av=na)

    # Summary from Name
    total <- nrow(filtered_name)
    wrong <- length(name_filter) - correct
    na <- total - correct - wrong
    summ_name <- c(total_name=total,correct=correct,wrong_gps=wrong,gps_not_av=na)

    # Plots
    if(plot) {
        par(mfrow=c(2,2))
        plot(sp$geom, main=name)
        plot(st_geometry(shapes[name,]), add=T)
        plot(valid_points[name_filter,], col=rgb(0,0,1,0.1), pch = 4, add=T)
        barplot(summ_name[2:4], main = "Coords of points filtered by municipality")
        barplot(summ_gps[2:4], main="Municipality of points filtered by GPS")
        # Third summary I guess
        barplot(sort(table(filtered_gps$NAME_2), decreasing = TRUE)[2:4], main="Top three wrong municipalities")
        if(save) {
            savePlot(paste0("plots/municipios/", tolower(plantR::rmLatin(name)), ".png"))
        }
    }

    c(summ_gps, summ_name[c(1,3:4)])
}

plotMun("Ubatuba")
plotMun("Campinas")
plotMun("Valinhos")
plotMun("Santo André")
plotMun("Santos")
plotMun("São Carlos")
plotMun("Campos Do Jordão")
plotMun("São Paulo")

tabs <- lapply(rownames(shapes), function(x) try(plotMun(x, plot=F)))
tabs <- do.call(rbind, tabs)
rownames(tabs) <- rownames(shapes)
write.csv(tabs, "data/derived-data/test_gps_municipalitites.csv")

t <- as.data.frame(tabs)
# Mean 67% and median 79%????
summary(t$correct/t$total_gps)
# Mean 19% and median 8% actual wrong names
summary(t$wrong_name/t$total_gps)
# In total, 9.8% of gps locations are on the wrong municipality
sum(t$wrong_name)/sum(t$total_gps)
