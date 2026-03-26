#' Generate UC string
#'
generate_uc_string <- function(x) {
    # String de regex para de/dos/da/das/,/-
    str_de <- "(,? ?( d[oae]s?)? ?| [-/] )"

    s <- tolower(x)
    # interchangeable names
    short_long <- tolower(paste0("(",uc_abbrevs$short,"|",uc_abbrevs$long,")"))
    for(n in short_long){
        s <- gsub(paste0("(^|\\s|-|\\.|\\|)",n,"(\\s|-|,)"),paste0("\\1",n,str_de),s,perl=T)
    }

    # de/dos/da pode estar incorreto ou faltante
    s <- gsub(" (d[oae]s?|[-–/]) ",str_de,s)
    # caracteres especiais podem estar incorretos ou faltantes
    s <- gsub("[éêẽè`]","[e _?]?",s)
    s <- gsub("[áâãà`]","[a _?]?",s)
    s <- gsub("[íîĩì`]","[i _?]?",s)
    s <- gsub("[óôõò`]","[o _?]?",s)
    s <- gsub("[úûüù`]","[u _?]?",s)
    s <- gsub("[ç`]","[c _?]?",s)
    s <- gsub("[-–/]","[ -–/]",s)
    s <- gsub("'","['’]?",s)
    s <- gsub("0","0?",s)
    s
}
