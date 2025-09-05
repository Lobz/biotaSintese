library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()
library(parallel)

br <- latamMap$brazil
spMap <- subset(br, stateProvince.correct == "sao paulo")
spMap1 <- list(sp=spMap)

occs <- saopaulo
occs <- remove_fields(occs)
occs <- formatTax(occs)
occs <- formatCoord(occs)
occs <- validateTax(occs)
occs <- validateLoc(occs)
load("data/derived-data/saopaulo_tmp_formatted.rda")
occs1 <- checkCoord(occs, high.map = spMap1)

which(duplicated(paste(bfoNames$tax.name,bfoNames$tax.authorship)))->i
bfoNames[i,]->p

testData <- data.frame(scientificName=p$tax.name,
                       scientificNameAuthorship=p$tax.authorship,
                       loc.correct = "brazil")

checkDist(testData)


lon = "decimalLongitude.new"; lat = "decimalLatitude.new";
    low.map = "plantR"; high.map = spMap1; country.shape = "country.correct";
    country.gazetteer = "country.gazet"; tax.name = "suggestedName";
    tax.author = "suggestedAuthorship"; sep = "_"; loc = "loc.correct";
    source = "bfo"; output = "same.col"

# problem is here
    chkd <- function(x){ checkDist(x, tax.name = tax.name,
        tax.author = tax.author,
        sep = sep, loc = loc, source = source)
    }
debug(checkDist)
x7 <- chkd(occs1)
undebug(checkDist)

x8 <- occs1[order(occs1$scientificName.new),]
x8 <- isolateProblemCases(x8, chkd)
dim(x8)
x8 <- isolateProblemCases(x8, chkd)
dim(x8)
debug(checkDist)
x9 <- x8[c(1,9,10,23),]
x10 <- chkd(x9)
undebug(checkDist)
occs2 <- validateCoord(occs, high.map = spMap1)

# check results of different options
dups.default <- validateDup(occs)
dim(dups.default)
head(dups.default)
dups.remove <- validateDup(occs, remove = T)
dim(dups.remove)
dups.overwrite <- validateDup(occs, overwrite = T)
dim(dups.overwrite)
dups.removeoverwrite <- validateDup(occs, remove = T, overwrite = T)
dim(dups.removeoverwrite)
dups.specific <- validateDup(occs, comb.fields = list(c("family", "col.last.name", "col.number")))
dim(dups.specific)

dups <- subset(dups.specific, scientificName.new != scientificName.new1)
head(dups)
dupsIDs <- dups$recordID

dt <- subset(occs, recordID %in% dupsIDs)
dt <- remove_fields(dt, c("country.correct", "stateProvince.correct", "municipality.correct", "locality.correct"))
dt <- validateCoord(dt)
dtt <- isolateProblemCases(dt, validateCoord)
if(nrow(dtt) < nrow(dt))
    dtt <- isolateProblemCases(dtt, validateCoord)

exID <- "FUEL_56352|RB_1508612"
subset(dups.default, dup.ID == exID)
subset(dups.remove, dup.ID == exID)
subset(dups.specific, dup.ID == exID)
subset(dups.overwrite, dup.ID == exID)
subset(dups.removeoverwrite, dup.ID == exID)

dim(occs)

occs <- validateDup(occs, tax.names=c(family = "family.new",
                                      species = "scientificName.new",
                                      det.name = "identifiedBy.new",
                                      det.year = "yearIdentified.new",
                                      tax.check = "tax.check",
                                      status = "scientificNameStatus",
                                      tax.auth = "scientificNameAuthorship",
                                      tax.rank = "taxon.rank",
                                      extra = "downloadedFrom"))
