
top_records <- function(x, n = 1) {
    # Get best specimen/identification combo
    x <- x[order(x$confidenceLocality == "High", x$tax.check, x$downloadedFrom=="Splink", as.numeric(x$year.new), as.numeric(x$yearIdentified.new), na.last=F, decreasing = T),]
    x <- x[!duplicated(x$scientificName.new),]
    x
}

format_list <- function(x, UC) {
    finalList <- data.frame(
        UC = UC,
        Grupos = x$group,
        Família = x$family.new,
        Gênero = x$genus.new,
        Espécie =  sub("^.+ ","",x$species.new),
        Autor = x$scientificNameAuthorship.new,
        Táxon_completo = paste(toupper(x$family.new), x$scientificNameFull),
        Barcode = ifelse(is.na(x$barcode),x$catalogNumber,x$barcode),
        BD_Origem = x$downloadedFrom,
        Herbário = x$collectionCode.new,
        Coletor = x$recordedBy.new,
        Número_da_Coleta = x$recordNumber,
        Origem_FFBr = x$origin,
        ConfiançaID = factor(x$tax.check, levels=c("unknown", "low", "medium", "high"), labels=c("Latão", "Bronze", "Prata", "Ouro")),
        ConfiançaLoc = x$confidenceLocality,
        Localidade = x$locality
    )
    finalList[order(finalList$Táxon_completo),]
}
