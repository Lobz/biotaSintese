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
    s <- x$Nome_UC[1]
    locs <- unlist(stringr::str_split(x$locality,",|[.]|;| - "))
    locs <- stringr::str_squish(locs)
    Local <- c(x$locality.new, x$locality, x$locality.scrap, x$NAME_3)
    Locality <- c(locs[!locs %in% Local], Local)
    Locality <- Locality[nchar(Locality) > 2 & tolower(Locality) != "sao paulo"]
    LT <- as.data.frame(table(Locality), stringsAsFactors=F, names = c("Locality", "Freq"))
    if(nrow(LT)==0) {
        return(NULL)
    }
    LT <- subset(LT, Freq >= nrow(x)/100 | Freq > 500)
    LT <- LT[order(LT$Freq, LT$Locality, decreasing = T),]
    LT <- LT[!duplicated(LT$Locality),]
    LT$Nome_UC <- s
    LT[,c(3,1,2)]

}

x <- locTable(dtTreated[[2]])
head(x)

tabs <- lapply(dtTreated, locTable)
TABS <- dplyr::bind_rows(tabs)
write.csv(TABS, "results/locationsTable.csv", row.names = F)

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
