
fixLocation <- function(saopaulo) {

    print(paste("Found", nrow(saopaulo), "records."))

    # get municipalities with unique name
    print("Loading municipality gazetteer...")
    munis <- read.csv("results/locations/municipalityGazetteer.csv")
    gazet = rbind(plantR:::gazetteer, munis)

    saopaulo$municipality <- sub("([ ,\\.^])sta\\.","\\1santa", saopaulo$municipality, ignore.case = T)
    saopaulo$locality <- sub("([ ,\\.^])sta\\.","\\1santa", saopaulo$locality, ignore.case = T)

    # load complementary gazetteer
    print("Loading extra gazetteer...")
    extra_gazet <- read.csv("results/locations/locGazetteer.csv")
    extra_gazet <- subset(extra_gazet, (!loc %in% gazet$loc) & (loc.correct %in% gazet$loc.correct), select = c("loc", "loc.correct"))
    extra_gazet_filled <- merge(extra_gazet, gazet[!duplicated(gazet$loc.correct),c(1,3:6)], by="loc.correct", all=F)[,names(gazet)]
    str(extra_gazet_filled)

    gazet <- rbind(gazet, extra_gazet_filled)

    print("Formatting loc...")
    saopaulo$country <- remove_spaces(saopaulo$country)
    saopaulo$stateProvince <- remove_spaces(saopaulo$stateProvince)
    saopaulo$municipality <- remove_spaces(saopaulo$municipality)
    saopaulo$locality <- remove_spaces(saopaulo$locality)
    saopaulo <- formatLoc(saopaulo, gazet = gazet)

    # gonna hand redo formatLoc
    # fixLoc is already done, thank you
    saopaulo$stateProvince.new <- fix_sp(saopaulo$stateProvince.new)
    saopaulo$municipality.new <- fix_sp(saopaulo$municipality.new)
    saopaulo$locality.new <- fix_sp(saopaulo$locality.new)
    saopaulo$country.new <- remove_spaces(saopaulo$country.new)
    saopaulo$stateProvince.new <- remove_spaces(saopaulo$stateProvince.new)
    saopaulo$municipality.new <- remove_spaces(saopaulo$municipality.new)
    saopaulo$locality.new <- remove_spaces(saopaulo$locality.new)

    print("Fixing country name...")
    saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("mog. mirim|campinas|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|cardoso|botucatu|moj. mirim|sao paulo",x$municipality.new), function(x) {

    x$country.new <- "brazil"
    x <- finLoc(x, gazet = gazet)
    })

    saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer %in% c("no_info") & grepl("mog. mirim|sorocaba|peruibe|ubatuba|campos d. jordao|cananeia|botucatu|moj. mirim|sao paulo",x$locality.new), function(x) {

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

    print("Fixing state name...")
    saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "country",finLoc,  gazet = gazet)

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

    x <- finLoc(x, gazet = gazet)

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

    x <- finLoc(x, gazet = gazet)
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

    x <- finLoc(x, gazet = gazet)
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

    # get admin names
    saopaulo <- addAdmin(saopaulo)

    print("Fixing municipality name...")
    saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "state" & !is.na(x$stateProvince.correct), function(x) {

    print(table(x$resolution.gazetteer))
    country <- x$country
    x$country <- x$country.correct
    state <- x$stateProvince
    x$stateProvince <- x$stateProvince.correct
    x <- formatLoc(x, gazet = gazet)
    # Return verbatim info to original
    x$stateProvince <- state
    x$country <- country
    print(table(x$resolution.gazetteer))
    x
    }, success_condition = function(x) x$resolution.gazetteer %in% c("county","locality"), label = "Using correct state name")

    print("Fixing locality name...")
    saopaulo <- tryAgain(saopaulo, function(x) x$resolution.gazetteer == "county" & !is.na(x$municipality.correct), function(x) {

    print(table(x$resolution.gazetteer))
    country <- x$country
    x$country <- x$country.correct
    state <- x$stateProvince
    x$stateProvince <- x$stateProvince.correct
    county <- x$municipality
    x$municipality <- x$municipality.correct
    x <- formatLoc(x, gazet = gazet)
    # Return verbatim info to original
    x$country <- country
    x$stateProvince <- state
    x$municipality <- county
    print(table(x$resolution.gazetteer))
    x
    }, success_condition = function(x) x$resolution.gazetteer %in% c("locality"), label = "Using correct state and municipality name")

    saopaulo <- tryAgain(saopaulo, function(x) x$loc == "brazil_NA_sao paulo" & !is.na(x$locality.new), finLoc, function(x) x$resolution.gazetteer=="locality")

    print("Subsetting to Brazil...")
    saopaulo <- addAdmin(saopaulo)

    tab(saopaulo$country.correct)
    tab(saopaulo$country.new[is.na(saopaulo$country.correct)])
    saopaulo <- subset(saopaulo, country.correct == "Brazil")

    # noCountry <- subset(saopaulo, is.na(country.correct))
    tab(saopaulo$stateProvince.correct)
    tab(saopaulo$municipality.new[is.na(saopaulo$stateProvince.correct)])
    print("Subsetting to São Paulo...")
    saopaulo <- subset(saopaulo, stateProvince.correct == "São Paulo" | is.na(stateProvince.correct))
    # sort(table(saopaulo$stateProvince.new, useNA="always"))
    # table(saopaulo$stateProvince.correct, useNA="always")
    # table(saopaulo$municipality.correct, useNA="always")
    # dim(saopaulo)

}