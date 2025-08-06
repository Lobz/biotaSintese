# Let's see what's going on with those stats


modCat <- list.files("results/checklist", full.names = T)
original <- list.files("results/allfields", full.names = T)

dataCat <- lapply(modCat, read.csv, na.strings = c("NA","","s.n.","s.c.","s.a."), colClasses = "character")
dtOrig <- lapply(original, read.csv, na.strings = c("NA",""), colClasses = "character")

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
prop.gps <- lapply(dtOrig, function(x) {
    c(High=sum(x$confidenceLocality=="High"),
    Low=sum(x$confidenceLocality=="Low"))

})
prop.gps <- as.data.frame(do.call(rbind, prop.gps))
prop.gps$Total <- prop.gps$High + prop.gps$Low
prop.gps$New <- prop.gps$Low > 10 & prop.gps$High < 10
prop.gps$Increase <- prop.gps$Low/prop.gps$High
prop.gps$Prop <- prop.gps$Low/prop.gps$Total

summary(prop.gps)

table(prop.gps$High==0)

summary(subset(prop.gps, High > 0))
summary(subset(prop.gps, High == 0))



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
