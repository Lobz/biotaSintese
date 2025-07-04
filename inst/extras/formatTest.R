
library(plantR) # used foi reading and cleaning occurrence data
occs_gbif <- rgbif2(species = "Rosaceae",
    country = "BR",
    n.records = 10)
occs_gbif$locality
occs <- formatDwc(gbif_data = occs_gbif,
    # drop = TRUE,
    # drop.opt = TRUE,
    drop.empty = TRUE
    )
occs
occs <- formatOcc(occs)
occs <- formatLoc(occs)
occs <- formatCoord(occs)
occs <- formatTax(occs)
occs <- validateLoc(occs)
occs <- validateCoord(occs) # resourse intensive - optimize?
occs <- validateTax(occs) # what the diff between this and formatTax?
occs <- validateDup(occs) # this removes dups? shouldn't we do this before other checks?
summ <- summaryData(occs)