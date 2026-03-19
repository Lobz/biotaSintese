#' Abbreviations
#'
#' These abbreviations are used for standardizing UC names and creating regexs
#' @export
uc_abbrevs <- data.frame(
    short= c("APAM", "APA", "RPPN", "ARIE", "RDS", "MNE", "FLONA", "FE", "PE", "PARNA", "PNM",
             "EEC|ESEC","ESEX","RESEX","REBIO","REVIS|RVS","MONA","ZVS"),
    long = c(
            "ÁREA DE PROTEÇÃO AMBIENTAL MARINHA",
            "ÁREA DE PROTEÇÃO AMBIENTAL",
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
