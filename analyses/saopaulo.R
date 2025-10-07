devtools::load_all()
library(geobr)
library(plantR)
library(parallel)

load("data/derived-data/saopaulo_occs.RData")

# gonna hand redo formatLoc
# fixLoc is already done, thank you
remove_spaces <- function(x) {
  x<- gsub(" +$","",x, perl=T)
  x<- gsub("^ +","",x, perl=T)
  x<- gsub("  +"," ",x, perl=T)
  x<- gsub(" +\\.",".",x, perl=T)
  x
}

remove_punct <- function(x) {
  x<- gsub("\\(.*\\)","",x, perl=T)
  x<- gsub("\\[.*\\]","",x, perl=T)
  x<- gsub(",|\\?|\\.|\\/|\\[|\\]|\\(|\\)|&"," ",x, perl=T)
  x
}

fix_sp <- function(x) {
    x <- gsub("^s(.?.?o?| #227;o) paulo", "sao paulo", x, ignore.case=T)
    gsub("(\\s|,|\\.|-)s(.?.?o?| #227;o) paulo", "\\1sao paulo", x, ignore.case=T)
}

finLoc <- function(x, ...) {
  print(table(x$resolution.gazetteer))
  # strLoc
  locs <- strLoc(x)
  locs$loc.string <- prepLoc(locs$loc.string) # priority string
  if ("loc.string1" %in% names(locs))
    locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string 1
  if ("loc.string2" %in% names(locs))
    locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string 2

  # getLoc
  locs <- getLoc(x = locs, ...)
  colunas <- c("loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
  colunas <- colunas[colunas %in% names(locs)]
  x[,colunas] <- NULL
  x <- cbind.data.frame(x,
                         locs[, colunas], stringsAsFactors = FALSE)
  x[x==""] <- NA
  print(table(x$resolution.gazetteer))
  x
}

saopaulo$stateProvince.new <- fix_sp(saopaulo$stateProvince.new)
saopaulo$municipality.new <- fix_sp(saopaulo$municipality.new)
saopaulo$locality.new <- fix_sp(saopaulo$locality.new)
saopaulo$country.new <- remove_spaces(saopaulo$country.new)
saopaulo$stateProvince.new <- remove_spaces(saopaulo$stateProvince.new)
saopaulo$municipality.new <- remove_spaces(saopaulo$municipality.new)
saopaulo$locality.new <- remove_spaces(saopaulo$locality.new)

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country",finLoc)

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("mog. mirim|campinas|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|cardoso|botucatu|moj. mirim",x$municipality.new), function(x) {

  x$country.new <- "brazil"
  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("mog. mirim|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|botucatu|moj. mirim",x$locality.new), function(x) {

  x$country.new <- "brazil"
  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("bra[sz]il",x$country.new), function(x) {

  x$country.new <- "brazil"
  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & x$stateProvince.new %in% c("sp","sao paulo","ceara","pernambuco","minas gerais"), function(x) {

  x$country.new <- "brazil"
  x <- finLoc(x)
})

# fix state name
saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country", function(x) {
  x$locality.new[grepl("arquipelago (de ?)sao pedro e sao paulo",x$stateProvince.new)] <- "arquipelago de sao pedro e sao paulo"
  x$stateProvince[grepl("arquipelago (de ?)sao pedro e sao paulo",x$stateProvince.new)] <- "pernambuco"
  x$stateProvince.new<- remove_punct(x$stateProvince.new)
  x$stateProvince.new<- gsub("state","",x$stateProvince.new)
  x$stateProvince.new<- gsub("estado","",x$stateProvince.new)
  x$stateProvince.new<- gsub("est +(d. )?","",x$stateProvince.new)
  x$stateProvince.new<- gsub("of|d. ","",x$stateProvince.new)
  x$stateProvince.new<- gsub("  +"," ",x$stateProvince.new)
  x$stateProvince.new<- fix_sp(x$stateProvince.new)
  x$stateProvince.new<- gsub("sao paulo sp?","sao paulo",x$stateProvince.new)
  x$stateProvince.new<- gsub("san pablo","sao paulo",x$stateProvince.new)
  x$stateProvince.new<- gsub("sao paolo","sao paulo",x$stateProvince.new)
  x$stateProvince.new<- remove_spaces(x$stateProvince.new)
  x$stateProvince.new[grepl("sao paulo",x$stateProvince.new)] <- "sao paulo"
  x$stateProvince.new[grepl("s #227;o paulo",x$stateProvince.new)] <- "sao paulo"

  # x$municipality.new <- NA

  x <- finLoc(x)

})

# Get those MEX002 cases
saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("country") & grepl("&SAO PAULO", x$locality, fixed=T), function(x) {
  x$municipality.new <- tolower(sub("&SAO PAULO","",x$locality))
  x$stateProvince.new <- "sao paulo"
  x$locality.new <- x$municipality.new

  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("country", "no_info"), function(x) {
  x$municipality.new[grepl("ubatuba",x$stateProvince.new)] <- "ubatuba"
  x$locality.new[grepl("ubatuba",x$stateProvince.new)] <- "ilha anchieta"
  towns <- grepl("mog. mirim|campinas|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|cardoso|botucatu|moj. mirim",x$stateProvince.new) & is.na(x$municipality.new)
  x$municipality.new[towns] <- x$stateProvince[towns]
  x$stateProvince.new[towns] <- "sao paulo"
  x$country.new[towns] <- "brazil"
  x$stateProvince.new[x$stateProvince.new=="sp"] <- "sao paulo"

  x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
  stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
  x$municipality.new <- gsub(stateStr,"",x$municipality.new)
  x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
  x$municipality.new<- remove_punct(x$municipality.new)
  x$municipality.new<- remove_spaces(x$municipality.new)

  x <- finLoc(x)
})

# get municipalitys with unique name
munis <- read.csv("results/locations/municipalityGazetteer.csv")
gazet = rbind(plantR:::gazetteer, munis)

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality.new), function(x) {
  # find state name in municipality name
  # x$stateProvince.new <- munis$name_state_norm[match(x$municipality, munis$name_muni)]

  x <- finLoc(x, gazet = gazet)
})

munis <- read.csv("results/locations/uniqueMunicipalities.csv")
saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality.new), function(x) {
  # find state name in municipality name
  x$stateProvince.new <- munis$stateProvince.new[match(x$municipality.new, munis$municipality.new)]

  x <- finLoc(x, gazet = gazet)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country" & is.na(x$municipality), function(x) {
  # find state name in state name
  muni <- tolower(rmLatin(x$stateProvince))
  x$stateProvince.new <- munis$stateProvince.new[match(muni, munis$municipality.new)]
  x$municipality.new <- muni

  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality.new), function(x) {
  # find state name in municipality name
  x$municipality.new <- fix_sp(x$municipality.new)
  x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
  stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
  x$municipality.new <- gsub(stateStr,"",x$municipality.new)
  x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
  x$municipality.new<- remove_punct(x$municipality.new)
  x$municipality.new<- remove_spaces(x$municipality.new)
  x$stateProvince.new <- munis$name_state_norm[match(x$municipality.new, tolower(rmLatin(munis$name_muni)))]

  x <- finLoc(x)
})

# saopaulo1 <- tryAgain(saopaulo, function(x) {x$resolution.gazetteer == "country" & !is.na(x$locality.new) & !is.na(x$stateProvince.new)}, function(x) {
#   # try without municipality
#   my_saved <- x$locality.new
#   x$locality.new <- NA
#   x <- finLoc(x)
#   print(my_saved)
#   print(length(my_saved))
#   # x$locality.new <- my_saved
# })
# table(saopaulo$country.new, useNA="always")
# sort(table(saopaulo$stateProvince.new, useNA="always"))

locs <- getAdmin(saopaulo$loc.correct)
names(locs)<-c("loc.correct.admin", "country.correct", "stateProvince.correct", "municipality.correct", "locality.correct", "source.loc")
saopaulo[,names(locs)] <- NULL
saopaulo <- cbind(saopaulo,locs)

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "state" & !is.na(x$stateProvince.correct), function(x) {

  print(table(x$resolution.gazetteer))
  country <- x$country
  x$country <- x$country.correct
  state <- x$stateProvince
  x$stateProvince <- x$stateProvince.correct
  x <- formatLoc(x)
  # Return verbatim info to original
  x$stateProvince <- state
  x$country <- country
  print(table(x$resolution.gazetteer))
  x
}, success_condition = function(x) x$resolution.gazetteer %in% c("county","locality"), label = "Using correct state name")

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "county" & !is.na(x$municipality.correct), function(x) {

  print(table(x$resolution.gazetteer))
  country <- x$country
  x$country <- x$country.correct
  state <- x$stateProvince
  x$stateProvince <- x$stateProvince.correct
  county <- x$municipality
  x$municipality <- x$municipality.correct
  x <- formatLoc(x)
  # Return verbatim info to original
  x$country <- country
  x$stateProvince <- state
  x$municipality <- county
  print(table(x$resolution.gazetteer))
  x
}, success_condition = function(x) x$resolution.gazetteer %in% c("locality"), label = "Using correct state and municipality name")

saopaulo <- addAdmin(saopaulo)

tab(saopaulo$country.correct)
tab(saopaulo$country.new[is.na(saopaulo$country.correct)])
saopaulo <- subset(saopaulo, country.correct == "Brazil")

# noCountry <- subset(saopaulo, is.na(country.correct))
tab(saopaulo$stateProvince.correct)
tab(saopaulo$municipality.new[is.na(saopaulo$stateProvince.correct)])
saopaulo <- subset(saopaulo, stateProvince.correct == "São Paulo" | is.na(stateProvince.correct))
# sort(table(saopaulo$stateProvince.new, useNA="always"))
# table(saopaulo$stateProvince.correct, useNA="always")
# table(saopaulo$municipality.correct, useNA="always")
# dim(saopaulo)

# Treat gps data
saopaulo <- formatCoord(saopaulo)
tab(saopaulo$origin.coord)
# Try again using verbatim coordinates -> this isn't working for some reason
# saopaulo <- tryAgain(saopaulo,
#     condition = function(x) {
#       x$origin.coord == "coord_gazet" & !is.na(x$verbatimLatitude) & !is.na(x$verbatimLongitude)
#     },
#     FUN = function(x) {
#         x$decimalLatitude <- x$verbatimLatidude
#         x$decimalLongitude <- x$verbatimLongidude
#         x <- formatCoord(x)
#         x
#     },
#     success_condition = function(x) x$origin.coord == "coord_original"

# )
tab(is.na(saopaulo$decimalLatitude.new))

table(is.na(saopaulo$locality), saopaulo$origin.coord)


# formatTax and validateTax
saopaulo <- getTaxonId(saopaulo)

# validate
saopaulo <- validateLoc(saopaulo)

map <- latamMap$brazil
map <- subset(map, NAME_1 == "sao paulo")
saopaulo <- validateCoord(saopaulo, high.map = map) # WORKING
saopaulo$recordID <- 1:nrow(saopaulo) # I need a unique ID for this
save(saopaulo,file="data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

sp_deduped <- validateDup(saopaulo, noNumb = NA, noYear = NA, noName = NA, prop=1,
          tax.names = c(family = "family.new", species = "scientificName.new", tax.auth =
    "scientificNameAuthorship.new", det.name = "identifiedBy.new", det.year =
    "yearIdentified.new", tax.check = "tax.check", tax.rank = "taxon.rank", status =
    "scientificNameStatus", id = "id", name.full = "scientificNameFull", gen = "genus.new", sp = "species.new"),
  geo.names = c(lat = "decimalLatitude.new", lon = "decimalLongitude.new", org.coord =
    "origin.coord", prec.coord = "precision.coord", geo.check = "geo.check", datum = "geodeticDatum"),
  loc.names = unique(c(loc.cols, loc.cols.plantR, loc.str = "loc.correct", res.gazet = "resolution.gazetteer", res.orig =
    "resol.orig", loc.check = "loc.check")))
names(sp_deduped)
save(sp_deduped,file="data/derived-data/reflora_gbif_jabot_splink_saopaulo_deduped.RData")

# str(saopaulo)
