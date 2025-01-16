ucs <- read.csv("~/BIOTA/unidades-de-conservacao/cnuc_2024_10.csv", sep=";", dec=",")
str(ucs)

table(ucs$Grupo)
table(ucs$Plano.de.Manejo)
table(ucs$UF)
ucs$in_SP <- grepl("SP",ucs$UF)

ucs_sp <- subset(ucs, in_SP)
table(ucs_sp$Grupo)
table(ucs_sp$Plano.de.Manejo)
sapply(ucs_sp, table)

ucs_sp[,c(2,4)]
subset(ucs, grepl("bauru",Nome.da.UC, ignore.case=T))
