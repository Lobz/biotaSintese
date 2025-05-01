
load("data/derived-data/occsR_reflora.RData")
occsR <- subset(occsR, grepl("sao paulo", stateProvince.new))
load("data/derived-data/occs_gbif_saopaulo.RData")
occs <- subset(occs, grepl("sao paulo", stateProvince.new))
reflora_gbif <- dplyr::bind_rows(occs, occsR)
dim(reflora_gbif)
dim(occsR)
save(reflora_gbif,"data/derived-data/reflora_gbif_saopaulo.RData")