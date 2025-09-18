devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)

tt <- list.files("results/total-treated", full.names = T)
dtTreated <- lapply(tt, read.csv, na.strings = c("NA",""), colClasses = "character")
nome_file <- sub(".*/","",tt)
nome_file <- sub(".csv","",nome_file)
names(dtTreated) <- nome_file

ucs <- read.csv("results/summary_multilist.csv")
ucs$nome_file <- gsub(" ","",tolower(rmLatin(ucs$Nome.da.UC)))
rownames(ucs) <- ucs$nome_file

for(x in nome_file){
    dtTreated[[x]]$Nome_UC <- ucs[x,1]
}

locTable <- function(x) {
    if(nrow(x)==0) {
        return(NULL)
    }
    x <- subset(x, confidenceLocality != "High")
    if(nrow(x)==0) {
        return(NULL)
    }
    x$locality <- gsub("Sr\\. $","Sr ",x$locality)

    locsList <- stringr::str_split(x$locality,",|[.]|;| - ")
    lens <- sapply(locsList, length)
    id <- sapply(1:nrow(x), function(i) rep(x$recordID[i],lens[i]))
    muns <- sapply(1:nrow(x), function(i) rep(x$municipality.correct[i],lens[i]))
    states  <- sapply(1:nrow(x), function(i) rep(x$stateProvince.correct[i],lens[i]))
    locs <- unlist(locsList)
    id <- unlist(id)
    muns <- unlist(muns)
    states <- unlist(states)
    length(muns)==length(locs)

    locs <- sub("\\.$","",locs)
    locs <- plantR:::squish(locs)
    Local <- c(x$locality.new, x$locality, x$locality.scrap, x$locality.correct)
    ID <- rep(x$recordID, 4)
    Muns <- rep(x$municipality.correct, 4)
    States <- rep(x$stateProvince.correct, 4)
    Local <- sub("\\.$","",Local)
    Localidade <- c(locs, Local)
    recordID <- c(id, ID)
    Municipio <- c(muns, Muns)
    Estado <- c(states, States)
    DT <- data.frame(recordID, Estado, Municipio, Localidade)
    if(nrow(DT)==0) {
        return(NULL)
    }
    DT <- subset(DT, nchar(Localidade) > 2 & !tolower(rmLatin(Localidade)) %in% c("sao paulo", "brasil", "brazil", "faz", "floresta ombrofila densa", "mata secundaria") & grepl("[A-z]",Localidade))
    if(nrow(DT)==0) {
        return(NULL)
    }

    DT$Municipio[is.na(DT$Municipio)] <- "" # So that aggregate doesn't exclude these cases
    DT$Estado[is.na(DT$Estado)] <- "" # So that aggregate doesn't exclude these cases

    LT <- aggregate(DT$recordID, list(Localidade = DT$Localidade, Municipio = DT$Municipio, Estado = DT$Estado), function(y) length(unique(y)))

    names(LT)[4] <- "Freq"
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Localidade, decreasing = T),]
    LT <- LT[!duplicated(tolower(rmLatin(paste(LT$Municipio, LT$Localidade)))),]

    LT$Nome_UC <- x$Nome_UC[1]
    LT[,c(5,1:4)]
}


locTable2 <- function(x) {
    if(nrow(x)==0) {
        return(NULL)
    }
    x <- subset(x, confidenceLocality != "High")
    if(nrow(x)==0) {
        return(NULL)
    }

    DT <- x[,c("recordID", "locality.new", "municipality.correct","stateProvince.correct")]
    DT2 <- DT
    DT2$locality.new <- x$locality.scrap
    DT <- rbind(DT, DT2)
    names(DT) <- c("recordID", "Localidade", "Municipio", "Estado")

    DT <- subset(DT, nchar(Localidade) > 2 & !tolower(rmLatin(Localidade)) %in% c("sao paulo", "brasil", "brazil", "faz", "floresta ombrofila densa", "mata secundaria") & grepl("[A-z]",Localidade))
    if(nrow(DT)==0) {
        return(NULL)
    }

    DT$Municipio[is.na(DT$Municipio)] <- "" # So that aggregate doesn't exclude these cases
    DT$Estado[is.na(DT$Estado)] <- "" # So that aggregate doesn't exclude these cases

    LT <- aggregate(DT$recordID, list(Localidade = DT$Localidade, Municipio = DT$Municipio, Estado = DT$Estado), function(y) length(unique(y)))

    names(LT)[4] <- "Freq"
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Localidade, decreasing = T),]
    LT <- LT[!duplicated(tolower(rmLatin(paste(LT$Municipio, LT$Localidade)))),]

    LT$Nome_UC <- x$Nome_UC[1]
    LT[,c(5,1:4)]
}

locTable3 <- function(x) {
    if(nrow(x)==0) {
        return(NULL)
    }
    x <- subset(x, confidenceLocality != "High" & resolution.gazetteer != "locality")
    if(nrow(x)==0) {
        return(NULL)
    }

    df <- x[,c("recordID",loc.cols)]
    # Formating the locality information
    occs.fix <- fixLoc(df)
    # Creating locality strings used to query the gazetteer
    occs.locs <- strLoc(occs.fix)
    occs.locs1 <- occs.locs
    # Final editing the locality strings (reduces variation in locality notation)
    occs.locs$loc.string <- prepLoc(occs.locs$loc.string)
    occs.locs$loc.string1 <- prepLoc(occs.locs$loc.string1)
    occs.locs$loc.string2 <- prepLoc(occs.locs$loc.string2)

    locs.basic <- getLoc(occs.locs)
    # This was a good idea but it doesn't work
    # locs.subs <- tryAgain(locs.basic, condition=function(x) x$resolution.gazetteer!="locality", FUN <- function(occs) {
    #     occs <- remove_fields(occs, c("loc", "loc.correct", "resolution.gazetteer", "latitude.gazetter", "longitude.gazetteer"))
    #     occs$loc.string <- gsub("\\s*,\\s*","_",occs$loc.string)
    #     occs$loc.string1 <- gsub("\\s*,\\s*","_",occs$loc.string1)
    #     occs$loc.string2 <- gsub("\\s*,\\s*","_",occs$loc.string2)
    #     occs <- getLoc(occs)
    #     occs
    # })
    # This does work tho
    locs.subs <- tryAgain(locs.basic, function(x) x$resolution.gazetteer!="locality", add_cols = T, FUN =function(occs) {
        occs <- remove_fields(occs, c("loc", "loc.correct", "resolution.gazetteer", "latitude.gazetter", "longitude.gazetteer"))
        occs$loc.orig <- occs$loc.string1
        occs$loc.string1 <- gsub("\\s*,.*","",occs$loc.string1)
        occs <- getLoc(occs)
        occs
    })
    locs.subs <- tryAgain(locs.subs, function(x) x$resolution.gazetteer!="locality", add_cols = T, FUN =function(occs) {
        occs <- remove_fields(occs, c("loc", "loc.correct", "resolution.gazetteer", "latitude.gazetter", "longitude.gazetteer"))
        occs$loc.orig <- occs$loc.string2
        occs$loc.string2 <- gsub("\\s*,.*","",occs$loc.string2)
        occs <- getLoc(occs)
        occs
    })

    locs.fixed <- subset(locs.subs, !is.na(loc.orig), select=c("loc.orig", "loc.correct"))
    locs.unfix <- subset(locs.subs, is.na(loc.orig))
    my_locs <- c(locs.unfix$loc.string, locs.unfix$loc.string1, locs.unfix$loc.string2)
    locs <- data.frame(loc.orig=my_locs, loc.correct=rep(locs.unfix$loc.correct,3))
    locs <- subset(locs, loc.orig != loc.correct)
    locs$loc.correct <- ""
    DT <- rbind(locs.fixed, locs)

    LT <- aggregate(DT$loc.orig, list(loc = DT$loc.orig, loc.correct = DT$loc.correct), length)

    names(LT)[3] <- "Freq"
    LT <- LT[order(LT$Freq, decreasing = T),]

    LT
}

x <- locTable2(dtTreated[[4]])
head(x)

for(x in dtTreated) LT <- locTable(x)
tabs <- lapply(dtTreated, locTable2)
TABS <- dplyr::bind_rows(tabs)
# write.csv(TABS, "results/locationsTable.csv", row.names = F)
TABS2 <- read.csv("results/locationsTable.csv")

TABS3 <- subset(TABS, Localidade %in% TABS2$Locality)
write.csv(TABS3, "results/locationsTable.csv", row.names = F)

total <- dplyr::bind_rows(dtTreated)
tabs_locs <- locTable3(total)
write.csv(tabs_locs,"results/locationsTable_getLoc.csv", row.names = F)



total <- dplyr::bind_rows(dtTreated)
nrow(total)
table(total$selectionCategory, useNA="always")

loctab <- unique(total[,c("Nome_UC","locality")])

## Second source of tab: names that "look like" they might be UCs

load("data/derived-data/reflora_gbif_jabot_splink_saopaulo.RData")

locs <- unique(unlist(sapply(dtTreated, function(x) unique(x$loc))))

locs

not_used <- subset(saopaulo, !loc %in% locs)

    terms_pattern <- "parque|reserva|reserve|fazenda|nacional|estadual|parna|flona|rebio|rppn|e\\.e\\.|biologica|ecologica|extrativista|park|farm|hacienda|estrada|rodovia|carretera|road|camino|sitio|mata|horto|jardim|campus|pico|serra|sierra|morro|chapada|colina|monumento|exp|floresta|refúgio"
possible_ucs <- grepl(terms_pattern, not_used$loc)
sort(table(not_used$loc[possible_ucs]))

not_good <- not_used$resolution.gazetteer != "locality" & not_used$loc != not_used$loc.correct
sort(table(not_used$loc[possible_ucs & not_good]))
dt <- not_used[possible_ucs & not_good,]
dt <- dt[order(dt$loc),]
write.csv(dt[, c(loc.cols, loc.cols.plantR)], "data/derived-data/temp_unused locations.csv", row.names=F)

matao <- dt[dt$loc=="brazil_sao paulo_matao",]
table(matao$municipality)
