#' Generate UC string
#'
#' @importFrom plantR rmLatin
generate_uc_string <- function(x) {
    s <- tolower(x)
    str_de <- "(,? ?( d[oae]s?)? ?| - )"
    # intercheangable names
    m <- tolower(paste0(uc_abbrevs$short,"|",uc_abbrevs$long))
    for(n in m)
        s <- sub(n,paste0("(",n,")",str_de),s,perl=T)

    # de/dos/da pode estar incorreto ou faltante
    s <- gsub(" (d[oae]s?|-) ",str_de,s)
    # caracteres especiais podem estar incorretos ou faltantes
    s <- gsub("[éêẽè`]","[éêẽèe _?]?",s)
    s <- gsub("[áâãà`]","[áâãàa _?]?",s)
    s <- gsub("[íîĩì`]","[íîĩìi _?]?",s)
    s <- gsub("[óôõò`]","[óôõòo _?]?",s)
    s <- gsub("[úûüù`]","[úûüùu _?]?",s)
    s <- gsub("[ç`]","[çc _?]?",s)
}

uc_abbrevs <- data.frame(
    short= c("APA", "RPPN", "ARIE", "RDS", "MNE", "FLONA", "PARNA", "PNM", "EE"),
    long = c("ÁREA DE PROTEÇÃO AMBIENTAL",
            "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL",
            "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO",
            "RESERVA DE DESENVOLVIMENTO SUSTENTÁVEL",
            "MONUMENTO NATURAL ESTADUAL",
            "FLORESTA NACIONAL",
            "PARQUE NACIONAL",
            "PARQUE NATURAL MUNICIPAL",
            "ESTAÇÃO ECOLÓGICA"))

#' Standardize UC Name
#'
#' Convert conservation unit name to long form and correct mistakes
#'
#' @param x UC names
standardize_uc_name <- function(x) {
    x <- sub("^AREA", "ÁREA", x)
    x <- sub("PATRIM.NIO", "PATRIMÔNIO", x)
    x <- sub("PATRIMÔNIO NATURA ", "PATRIMÔNIO NATURAL ", x, fixed = T)
    x <- sub(" AGUAS", " ÁGUAS", x, fixed = T)
    x <- sub("SITIO", "SÍTIO", x, fixed = T)
    x <- gsub("\"", "", x, fixed = T)
    for(i in 1:nrow(uc_abbrevs)){
        x <- sub(paste0("^",uc_abbrevs$short[i]), uc_abbrevs$long[i], x)
    }
    x
}

generate_filename <- function(x) {
    for(i in 1:nrow(uc_abbrevs)){
        x <- sub(paste0("^",uc_abbrevs$short[i]), uc_abbrevs$long[i], x)
    }
    x

}
