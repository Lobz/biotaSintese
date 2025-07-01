devtools::load_all()
library(geobr)
library(plantR)

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
saopaulo <- dplyr::bind_rows(saopaulo1,saopaulo2)

rm(gbif, reflora, jabot, splsaopaulo, goodNames, saopaulo1, saopaulo2)
gc()

saopaulo[saopaulo==""] <- NA

# Lets format this
saopaulo$recordedBy <- gsub("Eiten Eiten", "Eiten", saopaulo$recordedBy)
saopaulo$identifiedBy <- gsub("Ulloa Ulloa", "Ulloa", saopaulo$identifiedBy)
saopaulo <- formatOcc(saopaulo, noNumb = NA, noYear = NA, noName = NA)

# Subset country
saopaulo <- subset(saopaulo, is.na(country) | grepl("br", tolower(country), fixed=T))

x <- fixLoc(saopaulo)

###### PAUSE
save(saopaulo, file="temp.RData")
load("temp.RData")
formatLoc

noState <- which(saopaulo$resolution.gazetteer == "country")
x <- saopaulo[noState,]

unique(x[(grepl("s.?o paulo", x$locality.new)),]$stateProvince.new)

# gonna fucking hand redo formatLoc
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
    gsub("s.?.?o? paulo", "sao paulo", x)
}


# fix state name
  table(x$stateProvince.new)
  x$stateProvince.new[grepl("arquipelago (de ?)sao pedro e sao paulo",x$stateProvince.new)] <- "pernambuco"
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
  x$stateProvince.new[grepl("mogy mirim|campinas|sorocaba|peruibe",x$stateProvince.new)] <- "sao paulo"
  x$stateProvince.new[x$stateProvince.new=="sp"] <- "sao paulo"
  table(x$stateProvince.new)

# find state name in municipality name
x$municipality.new <- fix_sp(x$municipality.new)
x$stateProvince.new[grepl("sao paulo", x$municipality.new)] <- "sao paulo"
stateStr <- "state of sao paulo|sao paulo state|&sao paulo|estado de sao paulo|sao paulo -|estado sao paulo"
x$municipality.new <- gsub(stateStr,"",x$municipality.new)
x$municipality.new <- gsub(" -|- "," ",x$municipality.new)
x$municipality.new<- remove_punct(x$municipality.new)
x$municipality.new<- remove_spaces(x$municipality.new)



  # strLoc
  locs <- strLoc(x)
  locs$loc.string <- prepLoc(locs$loc.string) # priority string
  if ("loc.string1" %in% names(locs))
    locs$loc.string1 <- prepLoc(locs$loc.string1) # alternative string 1
  if ("loc.string2" %in% names(locs))
    locs$loc.string2 <- prepLoc(locs$loc.string2) # alternative string 2

  # getLoc
  locs <- getLoc(x = locs)
  table(locs$stateProvince.new == x$stateProvince.new, useNA="always")
  colunas <- c("loc", "loc.correct", "latitude.gazetteer", "longitude.gazetteer", "resolution.gazetteer")
  colunas <- colunas[colunas %in% names(locs)]
  x[,colunas] <- NULL
  x <- cbind.data.frame(x,
                         locs[, colunas], stringsAsFactors = FALSE)
  table(x$resolution.gazetteer)
x[x==""] <- NA

x <- x[,names(saopaulo)]

saopaulo[noState,] <- x

# table(saopaulo$country.new, useNA="always")
# sort(table(saopaulo$stateProvince.new, useNA="always"))

locs <- getAdmin(saopaulo$loc.correct)
names(locs)[1]<-"loc.correct.mun"
saopaulo[,names(locs)] <- NULL
saopaulo <- cbind(saopaulo,locs)

saopaulo <- subset(saopaulo, NAME_0 == "Brazil")

saopaulo <- subset(saopaulo, NAME_1 == "São Paulo" | is.na(NAME_1))
# sort(table(saopaulo$stateProvince.new, useNA="always"))
# table(saopaulo$NAME_1, useNA="always")
# table(saopaulo$NAME_2, useNA="always")
# dim(saopaulo)


save(saopaulo,file="data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")