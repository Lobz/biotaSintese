library(stringr)
library(dplyr)

parseRefloraCatUC <- function(data) {
    # data$Gênero[952] %>% stringr::str_to_title()
    print(paste0('n. registros: ', nrow(data)))
    print(paste0('n. colunas: ', ncol(data)))

    # separa e transforma as coordeadas deg_min_sec em graus decimais
    latitude <- gsub('?|\'|\"', '', data$`Latitude Mínima`)
    longitude <- gsub('?|\'|\"', '', data$`Longitude Mínima`)

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
    decimalLatitude <- as.vector(sapply(latitude, measurements::conv_unit, from = 'deg_min_sec', to = 'dec_deg'), mode = 'character')
    decimalLongitude <- as.vector(sapply(longitude, measurements::conv_unit, from = 'deg_min_sec', to = 'dec_deg'), mode = 'character')

    data$Ctrl_scientificNameOriginalSource <- NA
    data$Ctrl_scientificNameOriginalSource <- data$`Nome Científico`

    data <- data %>%
        dplyr::mutate(
        `Gênero` = `Gênero` %>% stringr::str_to_title(),
        `Espécie` = `Espécie` %>% stringr::str_to_lower(),
        `Família` = `Família` %>% stringr::str_to_title()) %>%
        dplyr::mutate(
        occurrenceID = `Código de Barra`,
        source = 'reflora',
        comments = '',

        scientificName = ifelse(`Rank:`=='Família',`Família`,
            ifelse(`Rank:`=='Gênero',`Gênero`,
            ifelse(`Rank:`=='Espécie',paste0(`Gênero`,' ',`Espécie`),
            ifelse(`Rank:`=='Variedade',paste0(`Gênero`,' ',`Espécie`, ' var. ', `subsp./var./forma`),
            ifelse(`Rank:`=='Subespécie',paste0(`Gênero`,' ',`Espécie`, ' subsp. ', `subsp./var./forma`),
            ifelse(`Rank:`=='Forma',paste0(`Gênero`,' ',`Espécie`, ' form. ', `subsp./var./forma`),
            '')))))),

        scientificNameAuthorship = `Autor do Táxon`,

        # taxonRank = `Rank:`,

        taxonRank = ifelse(`Rank:`=='Família','FAMILY',
            ifelse(`Rank:`=='Gênero','GENUS',
            ifelse(`Rank:`=='Espécie','SPECIES',
            ifelse(`Rank:`=='Variedade', 'VARIETY',
            ifelse(`Rank:`=='Subespécie', 'SUBSPECIES',
            ifelse(`Rank:`=='Forma', 'FORM',
            "")))))),
        institutionCode = "",
        collectionCode = `Herbário de Origem`,
        catalogNumber = `Código de Barra`,

        identificationQualifier = `Qualificador da Determinação (cf., aff. ou !)`,
        identifiedBy = Determinador,
        dateIdentified = `Data da Determinação`, #aqui

        typeStatus = Typus,

        recordNumber = `Número da Coleta`,
        recordedBy  = `Coletor`,

        year = substr(`De:`, 7, 12),
        month = substr(`De:`, 4, 5),
        day = substr(`De:`, 1, 2),

        country = `País`,
        stateProvince = Estado,
        municipality = `Município`,
        locality = `Descrição da Localidade`,

        decimalLatitude = decimalLatitude,
        decimalLongitude = decimalLongitude,
        occurrenceRemarks = `Descrição da Planta`,
        fieldNotes = `Observações`)
    return(data)
}