
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
shapes <- read_municipality("SP", year = 2024)

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
    # othermuns <- unique(filtered_gps$municipality.correct)
    # plot(filtered_gps, add=T, col = "blue")
    filtered_name <- saopaulo[which(tolower(saopaulo$municipality.correct)==tolower(name)),]
    if(nrow(filtered_name) == 0) {
        print(paste("Zero matches:", name))
        print(sort(table(filtered_gps$municipality.correct)) )
    }
    name_filter <- which(tolower(valid_points$municipality.correct)==tolower(name))

    # Summary from GPS
    total <- nrow(filtered_gps)
    correct <- length(intersect(name_filter, gps_filter))
    na <- sum(is.na(filtered_gps$municipality.correct))
    wrong <- total - correct - na
    summ_gps <- c(total_gps=total,correct=correct,wrong_name=wrong,name_not_av=na)

    # Summary from Name
    total <- nrow(filtered_name)
    wrong <- length(name_filter) - correct
    na <- total - correct - wrong
    summ_name <- c(total_name=total,correct=correct,wrong_gps=wrong,gps_not_av=na)

    # Plots
    if(plot & total > 0) {
        tryCatch( {

        if(save) {
            png(paste0("plots/municipios/", tolower(plantR::rmLatin(name)), ".png"), height=480, width=640)
        }
        par(mfrow=c(2,2))
        plot(sp$geom, main=name)
        plot(st_geometry(shapes[name,]), add=T)
        plot(valid_points$geometry[name_filter], col=rgb(0,0,1,0.1), pch = 4, add=T)
        barplot(sort(table(filtered_gps$municipality.correct[filtered_gps$municipality.correct != name]), decreasing = TRUE)[1:3], main="Top three wrong municipalities", las=1, horiz=T)
        barplot(summ_name[2:4], main = "Coords of points filtered by municipality")
        barplot(summ_gps[2:4], main="Municipality of points filtered by GPS")
        },
        finally={
            dev.off()
        })
    }

    c(summ_gps, summ_name[c(1,3:4)])
}

plotMun("Ubatuba")
plotMun("Campinas")
plotMun("Valinhos")
plotMun("Santo André")
plotMun("Santos")
plotMun("Embu das Artes")
plotMun("Campos Do Jordão")
plotMun("São Paulo")

tabs <- lapply(rownames(shapes), function(x) try(plotMun(x, plot=T)))
tabls <- sapply(tabs, function(x) if(class(x) == "integer") FALSE else TRUE)
tabs <- do.call(rbind, tabs)
rownames(tabs) <- rownames(shapes)
write.csv(tabs, "data/derived-data/test_gps_municipalitites.csv")
tabs <- read.csv("results/test_gps_municipalitites.csv")

t <- as.data.frame(tabs)
t$total <- t$total_gps + t$total_name - t$correct
t <- subset(t, total_name > 0)
summary(t)
ts <- subset(t, total_name > 20)
# Mean 67% and median 79%????
summary(t$correct/t$total_gps)
summary(t$correct/t$total_name)
boxplot(t$correct/t$total_gps)
summary(ts$correct/ts$total_gps)
plot(ts$correct/ts$total_gps ~ts$total_gps)
# Mean 19% and median 8% actual wrong names
summary(t$wrong_name/t$total_gps)
summary(t$wrong_name/t$total_name)
summary(t$name_not_av/t$total_gps)
summary(t$wrong_name/t$total_name)
# In total, 9.8% of gps locations are on the wrong municipality
sum(t$wrong_name)/sum(t$total_gps)

# Top records
subset(t, total > 50000)
subset(t, total < 20)
# Top correct perc (from GPS)
t$correct_perc <- t$correct / t$total_gps
hist(t$correct/t$total_gps, main="Correct / total GPS records", breaks=20)
t$X[t$total_name==0]
savePlot("plots/hist_municipios.png")
sort(t$correct_perc)
subset(t, correct_perc > .99)
subset(t, correct_perc < .001)
subset(t, total == 0)
subset(t, total_name == 0)
