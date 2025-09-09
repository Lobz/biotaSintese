# TO DOs

## Issues

### Gazetteer has duplicated loc.correct

[23] "brazil_sao paulo_porto ferreira_pe porto ferreira (antiga reserva estadual porto ferreira)"
[24] "brazil_sao paulo_porto ferreira_parque estadual porto ferreira"

### checkInverted error if col classes are wrong

Error in -tmp[, lon] : argumento inválido para operador unário

fix: use as.numeric

## locationTables

- remove all locations that match grep(uc_string) -> done?? maybe redo later
- search database for all occurrences of "estação experimental", "floresta d", 'eec", etc

## Listas de UCs

- Ver com colegas quais UCs devemos focar e quais devemos filtrar (ex: APAs que contém zonas industriais e metrópoles)
- Baixar dados do ICMBIO https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais https://www.gov.br/icmbio/pt-br/assuntos/dados_geoespaciais/mapa-tematico-e-dados-geoestatisticos-das-unidades-de-conservacao-federais
- Escrever script para combinar dados do ICMBIO com os do CNUC
- Enviar emails para: ICMBio, Reservas Votorantim, Instituto Florestal, Secretaria do Estado, etc, perguntando sobre dados de UCs
- Email para Tathy sobre UCs do IF e planos de manejo
- lista cncflora (perguntar Guilherme) tem no site do JBRJ
- ari de Teixeira oliveira-filho ou Danilo neves da UFMG - neotroptree (Renato tem contato)
- fazer lista preliminar de todos os possíveis nomes científicos (certos ou errados) baixando por exemplo do gbif, usar para fazer um filtro pelo bash por exemplo com grep para aplicar em bancos de dados imensos (ex: gbif sem filtros)
- fazer um arquivo com a lista de nomes que não foram identificados no plantr
- wdpa world database of protected areas wdpar no cran?

## Data cleaning

- todo: decide what to do with barcode NA

### Notes

- last update improved loc resolution for ~ 47k records (I had to redo all treatements so I don't have the exact number of cases where it resulted in worse resolution...)