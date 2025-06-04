#' Generate UC string
#'
#' @importFrom plantR rmLatin
generate_uc_string <- function(x) {
    s <- tolower(x)
    # intercheangable names
    shorts <- c("apa", "rppn", "arie", "rds", "mne")
    longs <- tolower(c("AREA DE PROTEÇÃO AMBIENTAL",
              "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL",
              "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO",
              "Reserva de Desenvolvimento Sustentável",
              "MONUMENTO NATURAL ESTADUAL"))
    m <- paste0(shorts,"|",longs)
    for(n in m)
        s <- sub(n,paste0("(",n,")"),s,perl=T)

    # de/dos/da pode estar incorreto ou faltante
    s <- gsub(" (d[oae]s?|-) ","(,? ?( d[oae]s?)? ?| - )",s)
    # caracteres especiais podem estar incorretos ou faltantes
    s <- gsub("[éêẽè`]","[éêẽèe _?]?",s)
    s <- gsub("[áâãà`]","[áâãàa _?]?",s)
    s <- gsub("[íîĩì`]","[íîĩìi _?]?",s)
    s <- gsub("[óôõò`]","[óôõòo _?]?",s)
    s <- gsub("[úûüù`]","[úûüùu _?]?",s)
    s <- gsub("[ç`]","[çc _?]?",s)
}
