
create_list <- function(x, UC) {
    # Get most recent specimen/identification combo
    splitdf <- by(x, x$scientificName.new, function(x) {
        x[order(x$tax.check, x$downloadedFrom=="SPLINK", as.numeric(x$year.new), as.numeric(x$yearIdentified.new), na.last=F, decreasing = T)[1],]
    }, simplify = F) #todo: optimize this
    x <- do.call(rbind, splitdf)

    finalList <- data.frame(
        UC = UC,
        #	Grupos
        Família = x$family.new,
        Gênero = x$genus.new,
        Espécie =  sub("^.+ ","",x$species.new),
        Autor = x$scientificNameAuthorship.new,
        Táxon_completo = x$scientificNameFull,
        Barcode = ifelse(is.na(x$barcode),x$catalogNumber,x$barcode),
        Origem = x$downloadedFrom,
        Herbário = x$collectionCode.new,
        Coletor = x$recordedBy.new,
        Número_da_Coleta = x$recordNumber,
        # Origem (segundo Flora & Funga do Brasil)
        ConfiançaID = factor(x$tax.check, levels=c("unknown", "low", "medium", "high"), labels=c("Latão", "Bronze", "Prata", "Ouro")),
        Localidade = x$locality
    )
    finalList[order(finalList$ConfiançaID, decreasing = T),]
}
