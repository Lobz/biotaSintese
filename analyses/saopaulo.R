
load("data/derived-data/occsR_reflora.RData")
occsR <- subset(occsR, grepl("sao paulo", stateProvince.new))
load("data/derived-data/occs_gbif_saopaulo.RData")
occs <- subset(occs, grepl("sao paulo", stateProvince.new))
reflora_gbif <- dplyr::bind_rows(occs, occsR)
load(file="data/derived-data/jabot_saopaulo_dwc.RData")
saopaulo <- dplyr::bind_rows(reflora_gbif, jabot)
save(saopaulo,file="data/derived-data/reflora_gbif_jabot_saopaulo.RData")
