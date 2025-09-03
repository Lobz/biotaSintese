devtools::load_all()
library(geobr)
library(plantR)
library(parallel)

load("data/raw-data/gbif_saopaulo_raw.RData")
gbif$downloadedFrom <- "GBIF"
goodNames <- names(gbif)
gbif <- remove_fields(gbif)
load("data/raw-data/reflora_all.RData")
reflora$downloadedFrom <- "Reflora"
reflora <- consolidateCase(reflora, goodNames)
goodNames <- union(goodNames, names(reflora))
reflora <- remove_fields(reflora)
load("data/derived-data/jabot_saopaulo.RData")
jabot$downloadedFrom <- "JABOT"
jabot <- consolidateCase(jabot, goodNames)
goodNames <- union(goodNames, names(jabot))
jabot <- remove_fields(jabot)
load("data/raw-data/spl_saopaulo.RData")
splsaopaulo$downloadedFrom <- "Splink"
splsaopaulo <- consolidateCase(splsaopaulo, goodNames)
splsaopaulo <- remove_fields(splsaopaulo)

# Join all this together
saopaulo1 <- formatDwc(gbif_data = gbif, user_data = jabot)
saopaulo2 <- formatDwc(splink_data = splsaopaulo, user_data = reflora)
saopaulo <- dplyr::bind_rows(saopaulo1, saopaulo2)

rm(gbif, reflora, jabot, splsaopaulo, goodNames, saopaulo1, saopaulo2)
gc()

# Subset country
saopaulo <- subset(saopaulo, is.na(country) | grepl("br", tolower(country), fixed=T))

# Standardize missing information
saopaulo[saopaulo==""] <- NA

# Lets format this
saopaulo <- formatOcc(saopaulo, noNumb = NA, noYear = NA, noName = NA)

# ###### PAUSE
# save(saopaulo, file="data/derived-data/temp.RData")
# load("data/derived-data/temp.RData")

saopaulo <- formatLoc(saopaulo)

# gonna hand redo formatLoc
# fixLoc is already done, thank you
remove_spaces <- function(x) {
  x<- gsub(" +$","",x, perl=T)
  x<- gsub("^ +","",x, perl=T)
  x<- gsub("  +"," ",x, perl=T)
  x
}
remove_punct <- function(x) {
  x<- gsub("\\(.*\\)","",x, perl=T)
  x<- gsub("\\[.*\\]","",x, perl=T)
  x<- gsub(",|\\?|\\.|\\/|\\[|\\]|\\(|\\)|&"," ",x, perl=T)
  x
}

fix_sp <- function(x) {
    gsub("s(.?.?o?| #227;o) paulo", "sao paulo", x)
}

saopaulo$stateProvince.new <- fix_sp(saopaulo$stateProvince.new)
saopaulo$locality.new <- fix_sp(saopaulo$locality.new)

finLoc <- function(x) {
  # strLoc
  locs <- strLoc(x)
  locs$loc.string <- prepLoc(locs$loc.string) # priority string
  if ("loc.string1" %in% names(locs))
    locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string 1
  if ("loc.string2" %in% names(locs))
    locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string 2

  # getLoc
  locs <- getLoc(x = locs)
  colunas <- c("loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
  colunas <- colunas[colunas %in% names(locs)]
  x[,colunas] <- NULL
  x <- cbind.data.frame(x,
                         locs[, colunas], stringsAsFactors = FALSE)
  x[x==""] <- NA
  x <- x[,names(saopaulo)]
}

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country",finLoc)

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
saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("country","state") & grepl("\\w&saopaulo", x$locality.new), function(x) {
  x$municipality.new <- sub("&saopaulo","",x$locality.new)
  x$stateProvince.new <- sub(".*&","",x$locality.new)
  x$locality.new <- x$municipality.new

  x <- finLoc(x)
})

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country", function(x) {
  x$municipality.new[grepl("ubatuba",x$stateProvince.new)] <- "ubatuba"
  x$locality.new[grepl("ubatuba",x$stateProvince.new)] <- "ilha anchieta"
  x$municipality.new[grepl("mogy mirim|campinas|sorocaba|peruibe|ubatuba",x$stateProvince.new) & is.na(x$municipality.new)] <- x$stateProvince[grepl("mogy mirim|campinas|sorocaba|peruibe|ubatuba",x$stateProvince.new) & is.na(x$municipality.new)]
  x$stateProvince.new[grepl("mogy mirim|campinas|sorocaba|peruibe|ubatuba|vicosa",x$stateProvince.new)] <- "sao paulo"
  x$stateProvince.new[x$stateProvince.new=="sp"] <- "sao paulo"
  table(x$stateProvince.new)

  x$municipality.new <- fix_sp(x$municipality.new)
  x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
  stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
  x$municipality.new <- gsub(stateStr,"",x$municipality.new)
  x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
  x$municipality.new<- remove_punct(x$municipality.new)
  x$municipality.new<- remove_spaces(x$municipality.new)

  x <- finLoc(x)
})

# get municipalitys with unique name
munis <- geobr::read_municipality()
munis <- subset(munis, !duplicated(name_muni))
states <- geobr::read_state()
munis$name_state <- states$name_state[match(munis$code_state, states$code_state)]
munis$name_state_norm <- tolower(rmLatin(munis$name_state))

saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality), function(x) {
  # find state name in municipality name
  x$stateProvince.new <- munis$name_state_norm[match(x$municipality, munis$name_muni)]

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
names(locs)[1]<-"loc.correct.mun"
saopaulo[,names(locs)] <- NULL
saopaulo <- cbind(saopaulo,locs)

saopaulo <- subset(saopaulo, NAME_0 == "Brazil" | is.na(NAME_0))

noCountry <- subset(saopaulo, is.na(NAME_0))

saopaulo <- subset(saopaulo, NAME_1 == "São Paulo" | is.na(NAME_1))
# sort(table(saopaulo$stateProvince.new, useNA="always"))
# table(saopaulo$NAME_1, useNA="always")
# table(saopaulo$NAME_2, useNA="always")
# dim(saopaulo)

# Treat gps data
saopaulo <- prepCoord(saopaulo)
# Try again using verbatim coordinates
saopaulo <- tryAgain(saopaulo,
    condition = function(x) {
      x$coord.check == FALSE
    },
    FUN = function(x) {
        x <- remove_fields(x, c("decimalLatitude.new", "decimalLongitude.new", "coord.check"))
        x <- prepCoord(x, lat = "verbatimLatitude", lon = "verbatimLongitude")
        names(x)[ncol(x)-2:1] <- c("decimalLatitude.new", "decimalLongitude.new")
        x
    }
)
saopaulo <- getCoord(saopaulo)

table(is.na(saopaulo$locality), saopaulo$origin.coord)

saopaulo <- validateLoc(saopaulo)

# First pass in formatTax
saopaulo <- formatTax(saopaulo, parallel = TRUE, cores = detectCores() - 1)

# validate coord
saopaulo <- validateCoord(saopaulo) # NOT WORKING

# str(saopaulo)

save(saopaulo,file="data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")
