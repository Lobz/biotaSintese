#' Generate UC string
#'
#' @importFrom plantR rmLatin
generate_uc_string <- function(x) {
    # String de regex para de/dos/da/das/,/-
    str_de <- "(,? ?( d[oae]s?)? ?| - )"

    s <- tolower(x)
    # interchangeable names
    short_long <- tolower(paste0("(",uc_abbrevs$short,"|",uc_abbrevs$long,")"))
    for(n in short_long){
        s <- gsub(paste0("(^|\\s|-|\\.)",n,"(\\s|-|,)"),paste0(n,str_de),s,perl=T)
    }

    # de/dos/da pode estar incorreto ou faltante
    s <- gsub(" (d[oae]s?|-) ",str_de,s)
    # caracteres especiais podem estar incorretos ou faltantes
    s <- gsub("[éêẽè`]","[éêẽèe _?]?",s)
    s <- gsub("[áâãà`]","[áâãàa _?]?",s)
    s <- gsub("[íîĩì`]","[íîĩìi _?]?",s)
    s <- gsub("[óôõò`]","[óôõòo _?]?",s)
    s <- gsub("[úûüù`]","[úûüùu _?]?",s)
    s <- gsub("[ç`]","[çc _?]?",s)
    s <- gsub("-","[ -]",s)
    s <- gsub("'","['’]?",s)
    s
}

uc_abbrevs <- data.frame(
    short= c("APA", "RPPN", "ARIE", "RDS", "MNE", "FLONA", "FE", "PE", "PARNA", "PNM",
             "EEC|ESEC","ESEX","RESEX","REBIO","REVIS|RVS","MONA","ZVS"),
    long = c("ÁREA DE PROTEÇÃO AMBIENTAL",
            "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL",
            "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO",
            "RESERVA DE DESENVOLVIMENTO SUSTENTÁVEL",
            "MONUMENTO NATURAL ESTADUAL",
            "FLORESTA NACIONAL",
            "FLORESTA ESTADUAL",
            "PARQUE ESTADUAL",
            "PARQUE NACIONAL",
            "PARQUE NATURAL MUNICIPAL",
            "ESTAÇÃO ECOLÓGICA",
            "ESTAÇÃO EXPERIMENTAL",
            "RESERVA EXTRATIVISTA",
            "RESERVA BIOLÓGICA",
            "REFÚGIO DE VIDA SILVESTRE",
            "MONUMENTO NATURAL",
            "ZONA DE VIDA SILVESTRE"
            ))

#' Standardize UC Name
#'
#' Convert conservation unit name to long form and correct mistakes
#'
#' @param x UC names
#'
#' @details In addition to correcting common mistakes and coverting abbreviations to long form, this function also removes connectors between
standardize_uc_name <- function(x) {
    x <- plantR:::squish(x)
    x <- sub("^AREA", "ÁREA", x)
    x <- sub(" AREA", "ÁREA", x)
    x <- sub("PATRIM.NIO", "PATRIMÔNIO", x)
    x <- sub("REFUGIO", "REFÚGIO", x)
    x <- sub("PATRIMÔNIO NATURA ", "PATRIMÔNIO NATURAL ", x, fixed = T)
    x <- sub(" AGUAS", " ÁGUAS", x, fixed = T)
    x <- sub("SITIO", "SÍTIO", x, fixed = T)
    # Remove problematic characters
    x <- gsub("\"", "", x, fixed = T)
    x <- gsub("’", "'", x, fixed = T)
    x <- gsub("\\s", " ", x, perl = T)
    x <- gsub(",.*","", x, perl = T)
    for(i in 1:nrow(uc_abbrevs)){
        x <- sub(paste0("^", uc_abbrevs$short[i]), uc_abbrevs$long[i], x)
        x <- sub(paste0(uc_abbrevs$long[i]," (D[OAE]S?|-) "), paste0(uc_abbrevs$long[i]," "), x)
    }
    x <- plantR:::squish(x)
    x
}

shorten_uc_name <- function(x) {
    L <- uc_abbrevs$long
    S <- sub("\\|.*","",uc_abbrevs$short)
    for(i in 1:nrow(uc_abbrevs)){
        x <- gsub(L[i], S[i], x)
    }
    x
}

slug <- function(x) {
    x <- shorten_uc_name(x)
    x <- plantR::rmLatin(x)
    x <- plantR:::squish(x)
    x <- gsub("\\s+","_",x)
    x
}
