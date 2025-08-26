# Let's see what's going on with those stats


modCat <- list.files("results/checklist", full.names = T)
original <- list.files("results/allfields", full.names = T)
tt <- list.files("results/total-treated", full.names = T)
nome_file <- sub(".*/","",original)
nome_file <- sub(".csv","",nome_file)

dataCat <- lapply(modCat, read.csv, na.strings = c("NA","","s.n.","s.c.","s.a."), colClasses = "character")
dtOrig <- lapply(original, read.csv, na.strings = c("NA",""), colClasses = "character")
dtTreated <- lapply(tt, read.csv, na.strings = c("NA",""), colClasses = "character")
names(dtOrig) <- nome_file

i <- 6
i <- i+1
dt <- dtOrig[[i]]
modCat[i]
table(dt$municipality)
sort(table(dt$locality))
locs <- unlist(stringr::str_split(dt$locality,",|[.]|;|-"))
locs <- stringr::str_squish(locs)
sort(table(locs))

length(dtOrig)

# proportion of gps vs text entries
selCats <- lapply(dtOrig, function(x) {
    x <- x$selectionCategory
    x <- factor(x, levels=c("coord_original", "coord_gazet", "locality_exact", "locality_high", "locality_medium"))
    summary(x)
})
selCats <- dplyr::bind_rows(selCats)
selCats$total <- rowSums(selCats)
props <- 100*selCats/selCats$total

summary(props)
summary(selCats)

summary(subset(props, selCats$total > 10))
summary(subset(selCats, total > 10))

colSums(selCats)
100*colSums(selCats)/sum(selCats$total)

original[which(props$locality_high==max(props$locality_high))]
original[which(props$locality_medium==max(props$locality_medium))]



# Proportion of each taxon rank
prop.gps <- lapply(dtOrig, function(x) {
    data.frame(High=sum(x$confidenceLocality=="High"),
    Low=sum(x$confidenceLocality=="Low"))
})


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
