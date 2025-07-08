#' @title Converts data from Reflora to DarwinCore
#'
#' @param data Data.table downloaded from Reflora
#'
#' @author Mali Oz Salles and Pablo Melo
#'
#' @importFrom measurements conv_unit
#' @importFrom stringr str_count
#' @importFrom dplyr mutate
#'
#' @encoding UTF-8
#'
#' @keywords internal
#'
parseReflora <- function(data) {
    print(paste0('n. registros: ', nrow(data)))
    print(paste0('n. colunas: ', ncol(data)))

    # Replace names with DWC names
    if(all(names_reflora$nome_reflora == names(data))) {
        names(data) <- names_reflora$nome_dwc
    } else {
        stop("Names are not the expected standard!")
    }

    # separa e transforma as coordeadas deg_min_sec em graus decimais
    latitude <- gsub('?|\'|\"', '', data$verbatimLatitude)
    longitude <- gsub('?|\'|\"', '', data$verbatimLongitude)

    #latitude sul (-)
    latitude<-ifelse(str_count(latitude,'S|s')>0,paste0('-',gsub('S|s','',latitude)),latitude)
    #latitude norte
    latitude<-ifelse(str_count(latitude,'N|n')>0,gsub('N|n','',latitude),latitude)

    #longitude oeste (-)
    longitude<-ifelse(str_count(longitude,'W|w')>0,paste0('-',gsub('W|w','',longitude)),longitude)
    #latitude norte
    longitude<-ifelse(str_count(longitude,'E|e')>0,gsub('E|e','',longitude),longitude)

    longitude<-gsub('º','',longitude)
    latitude<-gsub('º','',latitude)

    latitude <- ifelse(latitude=='-0 0 0 ',"",latitude)
    longitude <- ifelse(longitude=='-0 0 0 ',"",longitude)


    # convert from decimal minutes to decimal degrees
    decimalLatitude <- sapply(latitude, measurements::conv_unit, from = 'deg_min_sec', to = 'dec_deg')
    decimalLongitude <- sapply(longitude, measurements::conv_unit, from = 'deg_min_sec', to = 'dec_deg')

    data$taxonRank = factor(data$taxonRank,
        levels = c('Forma','Variedade','Subespécie','Espécie','Gênero','Família','Ordem', 'Classe', 'Filo', 'Reino'),
    labels = taxonRanks,
    ordered=TRUE)

    data <- data %>% dplyr::mutate(
        # source = 'reflora',
        # comments = '',

        scientificName = substr(verbatimScientificName, nchar(family)+2, nchar(verbatimScientificName)),


        year = substr(dateCollected, 7, 12),
        month = substr(dateCollected, 4, 5),
        day = substr(dateCollected, 1, 2),

        decimalLatitude = decimalLatitude,
        decimalLongitude = decimalLongitude,
        county = NA,
        institutionCode = collectionCode
    )

    return(data)
}
