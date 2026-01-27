devtools::load_all()
# Let's see what's going on with those stats
summ <- read.csv("results/summary_treatOccs.csv")
# Type of UC
summ$type <- factor(gsub("_.*","", slug(summ_ml$Nome.da.UC)))
summary(summ)

# Open data
modCat <- list.files("results/checklist", "*.csv", full.names = T)
original <- list.files("results/allfields", "*.csv", full.names = T)
tt <- list.files("results/total-treated", "*.csv", full.names = T)
nome_file <- sub(".*/","",original)
nome_file <- sub(".csv","",nome_file)

dtCat <- lapply(modCat, read.csv, na.strings = c("NA","","s.n.","s.c.","s.a."), colClasses = "character")
dtOrig <- lapply(original, read.csv, na.strings = c("NA",""), colClasses = "character")
dtTreated <- lapply(tt, read.csv, na.strings = c("NA",""), colClasses = "character")
names(dtOrig) <- names(dtCat) <- names(dtTreated) <- nome_file
Nome.da.UC <- sapply(dtOrig, function(x) x$Nome_UC[1])

# Number of UCs with at least one record found
length(dtTreated)
# Number of records in each list
hist(summ$NumRecords[summ$NumRecords>0 & summ$NumRecords < 300000], xlab= "Número de registros", ylab = "Frequência", breaks=20, main = "Distribuição do número de registros")
hist(log(summ$NumRecords[summ$NumRecords>0 & summ$NumRecords < 300000]), xlab= "Número de registros (log)", ylab = "Frequência", breaks=20, main = "Distribuição do número de registros")

# Number of records vc type
table(summ$type)
table(summ$NumRecords > 0, summ$type)
table(summ$NumRecords > 20, summ$type)
table(summ$NumRecords > 100, summ$type)
table(summ$NumRecords > 1000, summ$type)
table(summ$NumRecords > 10000, summ$type)

boxplot(NumRecords ~ factor(type), data = subset(summ, NumRecords > 0 & type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), log="y", xlab = "Tipo de UC", ylab = "Número de registros", main = "Número de registros por tipo de UC")
savePlot("plots/numRecords_log.png")
boxplot(NumRecords ~ factor(type), data = subset(summ, type %in% c("APA", "ARIE","EEC", "PE", "PNM", "RPPN")), xlab = "Tipo de UC", ylab = "Número de registros", main = "Número de registros por tipo de UC")
savePlot("plots/NumRecords.png")

# Number of taxons in each list
n_tax <- sapply(dtOrig, nrow)
table(n_tax > 20)
tranks <- make_summary(dtOrig, "taxon.rank", levels = taxonRanks, Nome.da.UC)
table(tranks$species > 100)
table(tranks$species > 1000)
hist(tranks$species, breaks=20)
# Number of high quality taxons in each list
confLoc <- make_summary(dtOrig, "confidenceLocality", levels=c("High", "Medium", "Low", "None"), UC=Nome.da.UC)
confTax <- make_summary(dtOrig, "tax.check", levels=c("high", "medium", "low", "unknown"), UC=Nome.da.UC)

length(dtOrig)
# proportion of entries listed in catalogoUCsBR
catalogo <- make_summary(dtCat, column="Já.listada", levels=c("Sim", "Não"))
tem_lista <- subset(catalogo, Sim>0)
head(tem_lista)

# proportion of gps vs text entries
selCats <- lapply(dtOrig, function(x) {
    x <- x$selectionCategory
    x <- factor(x, levels=c("coords_original", "coords_gazet", "locality_exact", "intersect_high", "intersect_medium"))
    summary(x)
})
selCats <- dplyr::bind_rows(selCats)
selCats <- cbind(Nome.da.UC,selCats)
summary(selCats)

m <- merge(summ_ml, confLoc, all=T)
summary(m)
m[is.na(m)] <- 0
write.csv(m, "results/summary_multilist.csv", row.names=F)

selCats$total <- rowSums(selCats)
props <- 100*selCats/selCats$total

summary(props)

summary(subset(props, selCats$total > 10))
summary(subset(selCats, total > 10))

colSums(selCats)
100*colSums(selCats)/sum(selCats$total)

original[which(props$intersect_high==max(props$intersect_high))]
original[which(props$intersect_medium==max(props$intersect_medium))]


    # Add info about being new to catalogo
    UC_catalogo <- subset(catalogoCompleto, grepl(Nome_UC, Unidade.Conservação, perl = T, ignore.case = T))
    speciesCatalogo <- unique(UC_catalogo$scientificNameFull)
    listed <- finalList$Táxon_completo %in% UC_catalogo$Táxon
    finalList[,"Já listada"] <- ifelse(listed, "Sim", "Não")


# Proportion of each taxon rank
prop.gps <- lapply(dtOrig, function(x) {
    data.frame(High=sum(x$confidenceLocality=="High"),
    Low=sum(x$confidenceLocality=="Low"))
})


total <- dplyr::bind_rows(dtTreated)

t <- plantR::validateTax(total, generalist = T)

summary(sapply(total, is.na))
table(total$downloadedFrom, useNA="always")

table(is.na(total$id))
table(total$tax.notes)

total <- unique(total)
sp <- split(total, total$tax.notes)
sapply(sp, nrow)

x <- sp[[1]]
x <- getTaxonId(x)
table(x$taxon.rank)
table(is.na(x$id))
table(x$id)
table(x$family)
table(x$scientificNameAuthorship)
table(x$family.new)

total <- getTaxonId(total)

names(sp)

1802/22494

missingID <- subset(total, is.na(id))

sort(table(missingID$family.new, useNA="always")) # 27 NA
sort(table(missingID$genus.new, useNA="always")) # 119 NA??
sort(table(missingID$species.new, useNA="always")) # 389 NA??
sort(table(missingID$scientificName.new, useNA="always")) # 389 NA??

library(plantR)

retaxed <- prepSpecies(missingID, db="fbo")
str(retaxed)

table(is.na(retaxed$id))



(76+575)/3000
