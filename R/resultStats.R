# Let's see what's going on with those stats

my_files <- list.files("results")

my_files <- my_files[startsWith(my_files, "checklist")]
modCat <- my_files[endsWith(my_files, "modeloCatalogo.csv")]
original <- my_files[!endsWith(my_files, "modeloCatalogo.csv")]

dataCat <- lapply(paste0("results/",modCat), read.csv, na.strings = c("NA","","s.n.","s.c.","s.a."), colClasses = "character")
dtOrig <- lapply(paste0("results/",original), read.csv, na.strings = c("NA",""), colClasses = "character")

length(dtOrig)

total <- dplyr::bind_rows(dtOrig)

summary(sapply(total, is.na))
table(total$downloadedFrom, useNA="always")

table(is.na(total$id))

1802/22494

missingID <- subset(total, is.na(id))

sort(table(missingID$family.new, useNA="always")) # 27 NA
sort(table(missingID$genus.new, useNA="always")) # 119 NA??
sort(table(missingID$species.new, useNA="always")) # 389 NA??
sort(table(missingID$scientificName.new, useNA="always")) # 389 NA??

library(plantR)

retaxed <- prepSpecies(missingID, db="fbo")
str(retaxed)

table(is.na(retaxed$id))



(76+575)/3000
