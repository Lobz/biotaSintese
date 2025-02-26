
devtools::load_all()

library("plantR")

data(example_intro)
occs_splink <- example_intro

head(occs_splink)

familia <- "Blechnaceae"
splinkkey <- 'eZOGLZyihOoCLlAWs3Tx'
occs_splink <- rspeciesLink(family = familia, key = splinkkey)
str(occs_splink)
dim(occs_splink)
table(is.na(occs_splink$locality))
# read data from file
gbif_raw <- readData("../../BIOTA/GBIF/0061636-241126133413365.zip")
str(gbif_raw)

occs <- formatDwc(gbif_data = gbif_raw[[1]])
str(occs)

# read data directly from GBIF
occs_gbif <- rgbif2(species = familia,
    country = "BR",
    stateProvince = "São Paulo",
    n.records = 450000)
dim(occs_gbif)
# Check GBIF docs - what's the diff between locality and verbatimLocality
# A: locality is what's left after removing country, state, mun. info
table(is.na(occs_gbif$locality), is.na(occs_gbif$verbatimLocality))
# And how is it possible to have only one of them ?
# A: gbif removes the duplicated data, usually
subset(occs_gbif, is.na(locality) &! is.na(verbatimLocality))$verbatimLocality
unique(subset(occs_gbif, !is.na(locality) & is.na(verbatimLocality))$locality)
t(subset(occs_gbif, !is.na(locality) &! is.na(verbatimLocality))
    [,c("verbatimLocality","locality")])
# Seems to me like we should use either one like
# occs$locality[is.na(occs$locality)] <- occs$verbatimLocality[is.na(occs$locality)]

head(occs_gbif)
occs <- formatDwc(gbif_data = occs_gbif, splink_data = occs_splink)
table(is.na(occs$locality))
sort(names(occs))
samp <- sample(1:nrow(occs), 12)
occs[samp,"locality"]

occs <- formatOcc(occs)
names(occs)
occs <- formatLoc(occs)
table(is.na(occs$locality))
# ok formatLoc solves this issue. of course
t(occs[samp,c("locality", "locality.new", "stateProvince", "municipality", "loc")])

occs[samp, 167:187]
occs <- formatCoord(occs)
names(occs)
occs[samp, 187:194]

occs <- formatTax(occs)
names(occs)
t(occs[samp,c("scientificName", "scientificName.new", "tax.notes")])

occs <- validateLoc(occs)
table(occs$loc.check)
# why are we losing mun to state?
(subset(occs, loc.check == "check_municip.2state")$locality)
# we have locs such as
# [1] "brazil_sao paulo_capao bonito mun"
#  [2] "brazil_sao paulo_paranapiacaba"
#  [3] "brazil_sao paulo_jaragua"
#  [4] "brazil_sao paulo_mooca"
#  [5] "brazil_sao paulo_mococa mun"
#  [6] "brazil_sao paulo_santa rosa viterebo"
#  [7] "brazil_sao paulo_sao roque fartura"
#  [8] "brazil_sao paulo_pico jaragua"
#  [9] "brazil_parana_sao joao pinhais"
# [10] "brazil_para_serra carajas"
# [11] "brazil_pernambuco_taguatinga"
# [12] "paraguay_central_acosta nu"
# [13] "brazil_minas gerais_dias tavares"
# [14] "brazil_minas gerais_conselheiro malta"
# [15] "brazil_minas gerais_parque nacional serra ca"
# [16] "brazil_minas gerais_parque florestal serra i"
# [17] "brazil_parana_guaraguacu"

# those are not municipalities

occs <- validateCoord(occs) # resourse intensive - optimize?
table(occs$geo.check)

occs <- validateTax(occs) # what the diff between this and formatTax?

# Top people with many determinations but not in the taxonomist list:

# |Identifier            | Records|
# |:---------------------|-------:|
# |Prado, J.             |     382|
# |Smith, A.R.           |     306|
# |Moran, R.C.           |     274|
# |Molino, S.            |     214|
# |Pietrobom-Silva, M.R. |     135|
# |Chambers, T.C.        |      76|
# |Rojas, A.             |      66|
# |Gonzatti, F.          |      57|
# |Almeida, T.E.         |      56|
# |Athayde, F.P.F.       |      49|

names(occs)
table(occs$tax.check)
table(occs$identifiedBy)

occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?

summ <- summaryData(occs)
#' |Type                     | Records|
# |:------------------------|-------:|
# |Unicates                 |    3708|
# |Duplicates               |    1994|
# |Unknown                  |    1180|
# |Total without duplicates |    5815|
# |Total with duplicates    |    6882|

# What is an unkown??

# =============
#  COLLECTIONS
# =============
# Number of biological collections: 88
# Number of collectors' names: 1416
# Collection years: 87-2024 (>90% and >50% after 1945 and 1994)

summary(as.integer(occs$year.new))
summary(as.integer(occs$year))
# --> what is 87? Is a check of dates missing? earliest collection is in 1814 not 87

# Top collections in numbers of records:
# |Collection | Records|
# |:----------|-------:|
# |MO         |    1738|
# |SJRP       |     728|
# |K          |     599|
# |E          |     578|
# |L          |     547|

# Top collectors in numbers of records:
# |Collector             | Records|
# |:---------------------|-------:|
# |Salino, A.            |     301|
# |Pietrobom-Silva, M.R. |     184|
# |Dittrich, V.A.O.      |      93|
# |Eiten, G.             |      82|
# |Athayde, F.P.F.       |      79|

# ----> note: this is collections, not determinations

# ==========
#  TAXONOMY
# ==========
# Number of families: 1
# Number of genera: 18
# Number of species: 204

# Top richest families:
# |family.new  |    N|   S|
# |:-----------|----:|---:|
# |Blechnaceae | 6882| 204|

# Top richest genera:
# |genus.new      |    N|   S|
# |:--------------|----:|---:|
# |Blechnum       | 3254| 125|
# |Parablechnum   |  697|  25|
# |Austroblechnum |  297|  16|
# |Lomariocycas   |  241|   9|
# |Lomaridium     |  247|   8|

# ===========
#  COUNTRIES
# ===========
# Number of countries: 18

# Top countries in numbers of records:
# |Country   | Records| Species|
# |:---------|-------:|-------:|
# |Brazil    |    3736|      91|
# |[Unknown] |    1128|      88|
# |Peru      |     451|      59|
# |Bolivia   |     343|      63|
# |Ecuador   |     316|      52|
# >

table(is.na(occs$country), is.na(occs$stateProvince))

flags <- summaryFlags(occs)

# ==================
#  DUPLICATE SEARCH
# ==================
# Records per strength of duplicate indication:

# |Strenght               | Records|
# |:----------------------|-------:|
# |0%                     |    3708|
# |25%                    |     385|
# |50%                    |       8|
# |75%                    |     168|
# |100%                   |    1433|
# |Cannot check (no info) |    1180|

# =====================
#  LOCALITY VALIDATION
# =====================
# Results of the locality validation:

# |Validation           | Records|
# |:--------------------|-------:|
# |probably ok          |    2964|
# |ok (same resolution) |    2876|
# |check (downgraded)   |    1035|
# |ok (upgraded)        |       4|
# |check (not found)    |       3|

# Details of the validation (original vs. validated localities):

# |original.resolution | no_info| country| stateProvince| municipality| locality|
# |:-------------------|-------:|-------:|-------------:|------------:|--------:|
# |no_info             |    1128|       0|             0|            0|        0|
# |country             |       0|     627|             0|            0|        1|
# |stateProvince       |       0|       3|            99|            1|        0|
# |municipality        |       1|       7|            17|          429|        2|
# |locality            |       2|      16|          1042|         2982|      525|

# =======================
#  COORDINATE VALIDATION
# =======================
# Valid coordinates per origin:

# |Validated |Origin       | Records|
# |:---------|:------------|-------:|
# |yes       |original     |    3958|
# |yes       |gazetter     |    1930|
# |no        |cannot_check |     994|

# Valid coordinates per resolution:

# |Validated |Resolution          | Records|
# |:---------|:-------------------|-------:|
# |yes       |ok_county           |    3477|
# |yes       |ok_state            |    1303|
# |no        |no_cannot_check     |     994|
# |yes       |ok_country          |     785|
# |yes       |ok_locality         |     139|
# |yes       |check_gazetteer     |     119|
# |yes       |shore               |      36|
# |yes       |bad_country[border] |      23|
# |yes       |open_sea            |       6|

# ======================
#  CULTIVATED SPECIMENS
# ======================
# Number of specimens from cultivated individuals:

# |Cultivated   | Records|
# |:------------|-------:|
# |probably not |    6880|
# |probably yes |       2|

# ======================
#  TAXONOMIC CONFIDENCE
# ======================
# Confidence level of the taxonomic identifications:

# |Confidence | Records|
# |:----------|-------:|
# |low        |    3346|
# |unknown    |    2415|
# |high       |    1121|
# >

# --> what is taxonomic confidence based on? proporção de registros da espécie validados por taxonomista especialista da família (que está na base de dados)

# the main course:
list <- checkList(occs,
            n.vouch=3, # max number of vouchers per species (hopefully it will order from best to worst?)
            type = "selected", # unsure what the options mean
            rm.dup = TRUE, # remove duplicates!! (does it unify duplicates??)
            rank.type = 5 # this controls voucher ranking I think?
            )
str(list)

# very confused by this output

saveData(occs)
