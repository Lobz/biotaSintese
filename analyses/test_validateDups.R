library(plantR) # used foi reading and cleaning occurrence data
library(stringr)
devtools::load_all()
library(parallel)

br <- latamMap$brazil
spMap <- subset(br, NAME_1 == "sao paulo")
spMap1 <- list(sp=spMap)

occs <- saopaulo
occs <- remove_fields(occs)
occs <- formatTax(occs)
occs <- formatCoord(occs)
occs <- validateTax(occs)
occs <- validateLoc(occs)
load("data/derived-data/saopaulo_tmp_formatted.rda")
occs1 <- checkCoord(occs, high.map = spMap1)

lon = "decimalLongitude.new"; lat = "decimalLatitude.new";
    low.map = "plantR"; high.map = spMap1; country.shape = "NAME_0";
    country.gazetteer = "country.gazet"; tax.name = "suggestedName";
    tax.author = "suggestedAuthorship"; sep = "_"; loc = "loc.correct";
    source = "bfo"; output = "same.col"

    x1 <- checkCoord(occs1, lon = lon, lat = lat, low.map = low.map,
        high.map = high.map, dist.center = FALSE, keep.cols = c("geo.check",
            country.shape, country.gazetteer))
    x2 <- checkBorders(x1, geo.check = "geo.check", country.shape = country.shape,
        country.gazetteer = country.gazetteer, output = output)
    x3 <- checkShore(x2, geo.check = "geo.check", lon = lon,
        lat = lat, output = output)
    x4 <- checkInverted(x3, country.gazetteer = country.gazetteer,
        output = output)
    good.col <- ifelse(output == "same.col", "geo.check", "geo.check.new")
    check_these <- grepl("invert_|trans", x4[, good.col], perl = TRUE)
    if (any(check_these)) {
        x4.1 <- x4[check_these, ]
        x4.1 <- x4.1[, -which(names(x4.1) %in% c("geo.check",
            country.shape, country.gazetteer))]
        x4.2 <- checkCoord(x4.1, lon = ifelse(output == "same.col",
            lon, paste0(lon, ".new")), lat = ifelse(output ==
            "same.col", lat, paste0(lat, ".new")), low.map = low.map,
            high.map = high.map, dist.center = FALSE, keep.cols = c("geo.check"))
        x4[check_these, good.col] <- paste0(x4.2$geo.check, gsub(".*(?=\\[)",
            "", x4[check_these, good.col], perl = TRUE))
    } else {
        lon.new <- ifelse(output == "same.col", lon, paste0(lon,
            ".new"))
        lat.new <- ifelse(output == "same.col", lat, paste0(lat,
            ".new"))
        x4[[lon.new]] <- x4[, lon, drop = TRUE]
        x4[[lat.new]] <- x4[, lat, drop = TRUE]
    }
    x4 <- x4[, -which(names(x4) %in% c(country.shape, country.gazetteer))]
    if (output == "new.col") {
        x4$border.check.new[x4$border.check.new %in% TRUE] <- "bad_country[border]"
        x4$border.check.new[x4$border.check.new %in% c(FALSE,
            "FALSE")] <- "bad_country"
        x4$shore.check.new[x4$shore.check.new %in% TRUE] <- "shore"
        x4$shore.check.new[x4$shore.check.new %in% c(FALSE, "FALSE")] <- "open_sea"
    }
    x5 <- getCult(x4)
    x6 <- checkOut(x5, lon = ifelse(output == "same.col", lon,
        paste0(lon, ".new")), lat = ifelse(output == "same.col",
        lat, paste0(lat, ".new")), tax.name = tax.name, geo.name = ifelse(output ==
        "same.col", "geo.check", "geo.check.new"), cult.name = "cult.check",
        clas.cut = 3, rob.cut = 16)
# problem is here
    chkd <- function(x){ checkDist(x, tax.name = tax.name,
        tax.author = tax.author,
        sep = sep, loc = loc, source = source)
    }
debug(checkDist)
x7 <- chkd(x6)
undebug(checkDist)
x8 <- isolateProblemCases(x6, chkd)
dim(x8)

x8 <- x8[order(x8$scientificName.new),]
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
dt <- remove_fields(dt, c("NAME_0", "NAME_1", "NAME_2", "NAME_3"))
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
