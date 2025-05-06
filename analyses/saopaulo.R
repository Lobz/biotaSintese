
library(geobr)
library(plantR)

load("data/derived-data/occsR_reflora.RData")

sp <- subset(occsR, (grepl("brazil", country.new) | country.new=="") & grepl("s.* paulo", stateProvince.new))

load("data/derived-data/occs_gbif_saopaulo.RData")
reflora_gbif <- dplyr::bind_rows(occs, sp)
load(file="data/derived-data/jabot_saopaulo_dwc.RData")
saopaulo <- dplyr::bind_rows(reflora_gbif, jabot)
rm(occs, occsR, reflora_gbif, jabot, sp)
gc()

saopaulo$scientificNameAuthorship[is.na(saopaulo$scientificNameAuthorship)] <-
saopaulo$scientificNameAuthorship.x[is.na(saopaulo$scientificNameAuthorship)]
saopaulo$scientificNameAuthorship[is.na(saopaulo$scientificNameAuthorship)] <-
saopaulo$scientificNameAuthorship.y[is.na(saopaulo$scientificNameAuthorship)]
saopaulo$scientificNameAuthorship.x <- NULL


# table(saopaulo$country.new, useNA="always")
# sort(table(saopaulo$stateProvince.new, useNA="always"))

locs <- getAdmin(saopaulo$loc.correct)
names(locs)[1]<-"loc.correct.mun"
saopaulo <- cbind(saopaulo,locs)

saopaulo <- subset(saopaulo, NAME_0 == "Brazil")

states <- geobr::grid_state_correspondence_table
otherstates <- (setdiff(states$name_state, "São Paulo"))

saopaulo <- subset(saopaulo, !NAME_1 %in% otherstates)
# sort(table(saopaulo$stateProvince.new, useNA="always"))
# table(saopaulo$NAME_1, useNA="always")
# table(saopaulo$NAME_2, useNA="always")
# dim(saopaulo)

save(saopaulo,file="data/derived-data/reflora_gbif_jabot_saopaulo.RData")
