library(readr)

gbifsp_texto <- readr::read_tsv("~/BIOTA/GBIF/0061630-241126133413365/occurrence.tsv", col_types = paste0(rep("c", 297),collapse=""))
gbifsp_texto <- remove_empty_cols(gbifsp_texto)
str(gbifsp_texto)
summary(gbifsp_texto)
dim(gbifsp_texto)
table(gbifsp_texto$stateProvince)
gbifsp_gps <- readr::read_csv("~/BIOTA/GBIF/0061636-241126133413365/occurrence.csv", col_types = paste0(rep("c", 297),collapse=""))
table(gbifsp_gps$stateProvince)
gbifsp_gps <- remove_empty_cols(gbifsp_gps)
str(gbifsp_gps)
dim(gbifsp_gps)
colns <- names(gbifsp_gps)
gbifsp_gps[250830,]

ids <- intersect(sort(gbifsp_gps$gbifID), sort(gbifsp_texto$gbifID))
length(ids)
all.equal(subset(gbifsp_texto, gbifID %in% ids), subset(gbifsp_gps, gbifID %in% ids))
gbif_total <- merge(gbifsp_texto, gbifsp_gps, all.x=T)
dim(gbif_total)
dim(gbifsp_gps)+dim(gbifsp_texto)

# linhas duplicadas
sum(duplicated(gbif_total$gbifID))
dups <- gbif_total$gbifID[duplicated(gbif_total$gbifID)]
dim(dups)

sum(duplicated(gbif_total))

# lugares
sort(table(gbif_total$stateProvince))
