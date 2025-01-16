devtools::load_all()



gbifsp_texto <- read.gbif("~/BIOTA/GBIF/0061630-241126133413365/occurrence.tsv")
gbifsp_texto <- filter_fields.gbif(gbifsp_texto)
gbifsp_texto <- remove_empty_cols(gbifsp_texto)
str(gbifsp_texto)
dim(gbifsp_texto)
sort(table(gbifsp_texto$stateProvince))
summary(gbifsp_texto$hasCoordinate == "true") # 281297 têm coordenada

gbifsp_gps <- read.gbif("~/BIOTA/GBIF/0061636-241126133413365/occurrence.tsv")
gbifsp_gps <- filter_fields.gbif(gbifsp_gps)
gbifsp_gps <- remove_empty_cols(gbifsp_gps)
sort(table(gbifsp_gps$stateProvince))
dim(gbifsp_gps) # 367790 linhas

# merge
ids <- intersect(sort(gbifsp_gps$gbifID), sort(gbifsp_texto$gbifID))
length(ids) # 272991 na interseção
gbif_total <- merge(gbifsp_texto, gbifsp_gps, all=T)
dim(gbif_total)
nrow(gbifsp_gps)+nrow(gbifsp_texto) - length(ids)

# linhas duplicadas
sum(duplicated(gbif_total$gbifID))

# lugares
sort(table(gbif_total$stateProvince))
