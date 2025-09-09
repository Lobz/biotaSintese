devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(geobr)

# Load what I already have

load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

splocs <- saopaulo[, loc.cols]

splocs <- formatLoc(saopaulo)
# Get those MEX002 cases
splocs <- tryAgain(splocs, function(x) x$resolution.gazetteer %in% c("country") & grepl("&SAO PAULO", x$locality, fixed=T), function(x) {
  x$municipality.new <- tolower(sub("&SAO PAULO","",x$locality))
  x$stateProvince.new <- "sao paulo"
  x$locality.new <- x$municipality.new

  x <- finLoc(x)
})
# get municipalitys with unique name
munis <- geobr::read_municipality(year=2024)
munis <- subset(munis, !duplicated(name_muni))
states <- geobr::read_state(year=2024)
munis$name_state <- states$name_state[match(munis$code_state, states$code_state)]
munis$name_state_norm <- tolower(rmLatin(munis$name_state))

splocs <- tryAgain(splocs, function(x) x$resolution.gazetteer == "country" & !is.na(x$municipality), function(x) {
  # find state name in municipality name
  x$stateProvince.new <- munis$name_state_norm[match(tolower(rmLatin(x$municipality)), tolower(rmLatin(munis$name_muni)))]

  x <- finLoc(x)
})

splocs <- tryAgain(splocs, function(x) x$resolution.gazetteer == "country" & is.na(x$municipality), function(x) {
  # find state name in state name
  muni <- tolower(rmLatin(x$stateProvince))
  state <- munis$name_state_norm[match(muni, tolower(rmLatin(munis$name_muni)))]
  x$municipality.new <- muni
  x$stateProvince.new <- state

  x <- finLoc(x)
})

res.old <- factor(saopaulo$resolution.gazetteer, levels=c("locality","county","state","country"), ordered = T)
res.new <- factor(splocs$resolution.gazetteer, levels=c("locality","county","state","country"), ordered = T)

table(res.old, res.new)

better <- res.new < res.old
worse <- res.new > res.old
same <- res.old == res.new

sum(better)
sum(worse)
sum(same)

i<-10
saopaulo[which(worse & saopaulo$resolution.gazetteer=="county")[1:10+i],c(loc.cols, loc.cols.plantR)]
splocs[which(worse & saopaulo$resolution.gazetteer=="county")[1:10+i],c(loc.cols, loc.cols.plantR)]

x <- saopaulo
saopaulo[better, names(splocs)] <- splocs[better, ]
saopaulo <- x
save(saopaulo,file="data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

formatLoc(s)
save(splocs, file="tests/test-data/splocs_test_updates.Rdata")

df <- data.frame(country = "Brazil",
        stateProvince = c("Minas Gerais state", "state of Minas Gerais", NA, NA,NA,NA, NA, NA),
        municipality = NA,
        locality = c(NA, NA,
                     "Estado do Rio de Janeiro: Paraty",
                     "Brasil – Estado de São Paulo: Paranapiacaba, Estação Biológica.",
                     "State of Sao Paulo. Municipio de Sao Palo: Parque do Estado",
                     "São Paulo ad urbem Santos in prov. S.Pauli",
                     "Mun. de Cunha - prov. de São Paulo",
                     "Municipio de Cunha na provincia de São Paulo"))
fixLoc(df)
