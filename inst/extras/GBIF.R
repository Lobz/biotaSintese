gbifsp_texto <- read.csv("~/BIOTA/GBIF/0061630-241126133413365/occurrence.tsv", sep="\t")
gbifsp_gps <- read.csv("~/BIOTA/GBIF/0061636-241126133413365/occurrence.tsv", sep="\t")
str(gbifsp_gps)

gbif_total <- merge(gbifsp_gps, gbifsp_texto, all=T)
dim(gbif_total)
dim(gbifsp_gps)+dim(gbifsp_texto)

# linhas duplicadas
sum(duplicated(gbif_total$gbifID))
dups <- gbif_total$gbifID[duplicated(gbif_total$gbifID)]
dim(dups)

sum(duplicated(gbif_total))
