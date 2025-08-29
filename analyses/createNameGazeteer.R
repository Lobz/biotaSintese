devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(stringr)
library(sf)

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
    muns <- sapply(1:nrow(x), function(i) rep(x$NAME_2[i],lens[i]))
    states  <- sapply(1:nrow(x), function(i) rep(x$NAME_1[i],lens[i]))
    locs <- unlist(locsList)
    id <- unlist(id)
    muns <- unlist(muns)
    states <- unlist(states)
    length(muns)==length(locs)

    locs <- sub("\\.$","",locs)
    locs <- plantR:::squish(locs)
    Local <- c(x$locality.new, x$locality, x$locality.scrap, x$NAME_3)
    ID <- rep(x$recordID, 4)
    Muns <- rep(x$NAME_2, 4)
    States <- rep(x$NAME_1, 4)
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

x <- locTable(dtTreated[[4]])
head(x)

for(x in dtTreated) LT <- locTable(x)
tabs <- lapply(dtTreated, locTable)
TABS <- dplyr::bind_rows(tabs)
# write.csv(TABS, "results/locationsTable.csv", row.names = F)
TABS2 <- read.csv("results/locationsTable.csv")

TABS3 <- subset(TABS, Localidade %in% TABS2$Locality)
write.csv(TABS3, "results/locationsTable.csv", row.names = F)

# We should generate a gazeteer for everyone
# Point data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)

dt <- data.frame(country="BR", stateProvince="SP", municipality=ucs$Municípios.Abrangidos, locality=ucs$Nome.da.UC)
dt$municipality <- gsub("\\(.*\\)", "", dt$municipality)
dt <- formatLoc(dt)


total <- dplyr::bind_rows(dtTreated)
nrow(total)
table(total$selectionCategory, useNA="always")

loctab <- unique(total[,c("Nome_UC","locality")])


fromLoc <- subset(total, selectionCategory != "coord_original")
fromCoord <- subset(total, selectionCategory == "coord_original")

table(fromCoord$NAME_0, useNA="always")
table(fromCoord$stateProvince.new, useNA="always")
table(fromCoord$NAME_1, useNA="always")
table(is.na(fromCoord$NAME_2), is.na(fromCoord$locality), useNA="always")
table(is.na(fromCoord$locality), useNA="always")
table(is.na(fromCoord$NAME_2), useNA="always")

fromCoord <- remove_fields(fromCoord, c("NAME_0", "NAME_1", "NAME_2", "NAME_3"))
fromCoord <- checkCoord(fromCoord, dist.center = F)
