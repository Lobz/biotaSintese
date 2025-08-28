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
    x$locality <- gsub("Sr\\. $","Sr ",x$locality)

    locs <- unlist(stringr::str_split(x$locality,",|[.]|;| - "))
    locs <- sub("\\.$","",locs)
    locs <- plantR:::squish(locs)
    Local <- c(c(x$locality.new, x$locality, x$locality.scrap)[is.na(x$NAME_3)],x$NAME_3)
    Local <- sub("\\.$","",Local)
    Locality <- c(locs[!locs %in% Local], Local)
    Locality <- Locality[nchar(Locality) > 2 & tolower(rmLatin(Locality)) != "sao paulo"]
    Locality <- Locality[!tolower(rmLatin(Locality)) %in% c("sao paulo", "brasil", "brazil", "faz", "floresta ombrofila densa", "mata secundaria")]
    LT <- as.data.frame(table(Locality), stringsAsFactors=F, names = c("Locality", "Freq"))
    if(nrow(LT)==0) {
        return(NULL)
    }
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Locality, decreasing = T),]
    LT <- LT[!duplicated(tolower(LT$Locality)),]

    LT$Nome_UC <- x$Nome_UC[1]
    LT[,c(3,1,2)]

}

x <- locTable(dtTreated[[2]])
head(x)

tabs <- lapply(dtTreated, locTable)
TABS <- dplyr::bind_rows(tabs)
# write.csv(TABS, "results/locationsTable.csv", row.names = F)
TABS2 <- read.csv("results/locationsTable.csv")

TABS3 <- merge(TABS, TABS2, sort=F)
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
