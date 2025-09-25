devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(sf)
library(geobr)

# We should generate a gazeteer for the UCs

# Whenever possible, we want to extract coordinates from the cnuc data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes2 <- st_read("data/raw-data/shp_cnuc_2025_08/cnuc_2025_08.shp")
shapes$nome_uc_standard <- standardize_uc_name(shapes$nome_uc)
shapes2$nome_uc_standard <- standardize_uc_name(shapes2$nome_uc)
shapes[, c("uc_id", "cd_cnuc", "pl_manejo", "cria_ato", "outro_ato", "ppgr")] <- NULL
shapes2[, c("uc_id", "cd_cnuc", "pl_manejo", "cria_ato", "outro_ato", "ppgr")] <- NULL

head(shapes)

all_ucs <- shapes

# Filter only valid shapes
is_valid <- st_is_valid(shapes)
is_valid2 <- st_is_valid(shapes2)
tab(is_valid)
tab(is_valid2)
sort(shapes$nome_uc_standard[!is_valid])
shapes$is_valid <- is_valid
sort(shapes2$nome_uc_standard[!is_valid2])
sort(shapes2$uf[!is_valid2])
shapes2$is_valid <- is_valid2

# Merge data from both sources
in_both <- intersect(shapes$nome_uc_standard[is_valid], shapes2$nome_uc_standard[is_valid2])
dim(subset(shapes, nome_uc_standard %in% in_both)) # 2962
dim(subset(shapes2, nome_uc_standard %in% in_both)) #2964

in_old <- subset(shapes, !nome_uc_standard %in% in_both)
in_new <- subset(shapes2, !nome_uc_standard %in% in_both)
common <- subset(shapes2, nome_uc_standard %in% in_both)

all_shapes <- dplyr::bind_rows(common, in_old)
all_shapes <- dplyr::bind_rows(all_shapes, in_new)

dim(all_shapes)

# We will NOT be trusting the cnuc text data for what municipalities are included in each UC
# Instead, we will use the shape data and compare it with geobr data

# Get relevant dataset info
datasets <- list_geobr()

# Read state data from geobr
state_info <- datasets[datasets[,1]=="`read_state`",]
state_latest_year <- sub(".* ","",state_info$years)
state <- geobr::read_state(year = state_latest_year)
head(state)

# Since state info is more reliable, we'll be taking single-state UCs at their word
multi <- grepl(",", shapes$uf)
table(multi)
multi_state <- all_shapes[multi,]
single_state <- all_shapes[!multi,]
dim(single_state)
head(single_state)

# Name of state
single_state$name_state <- state$name_state[match(rmLatin(single_state$uf), toupper(rmLatin(state$name_state)))]

# Intersect state shapes with UC shapes
inter_state <- st_intersection(state, multi_state)
head(inter_state)

# Rejoin
shapes <- dplyr::bind_rows(single_state, inter_state)
table(shapes$name_state, useNA="always")

save(shapes, file="data/derived-data/intersected_shapes.rda")

# Read munis data from geobr
munis_info <- datasets[datasets[,1]=="`read_municipality`",]
munis_latest_year <- sub(".* ","",munis_info$years)
munis <- geobr::read_municipality(year = munis_latest_year)
munis[ ,c("code_state","abbrev_state","code_region","name_region")] <- NULL
head(munis)

# Split into states and intersect
munis_by_state <- split(munis, munis$name_state)
str(munis_by_state)
head(munis_by_state[[1]])
names(munis_by_state)

ucs_by_state <- split(shapes, inter_state$name_state)
head(ucs_by_state[[1]])
names(ucs_by_state)


inter_munis_by_state <- list()

for(i in 1:length(ucs_by_state)) {
    try(
        inter_munis_by_state[[i]] <- st_intersection(munis_by_state[[i]], ucs_by_state[[i]])
    )
}

names(inter_munis_by_state) <- names(ucs_by_state)

rbind(sapply(inter_munis_by_state, nrow), sapply(ucs_by_state, nrow))

inter_munis <- dplyr::bind_rows(inter_munis_by_state)
dim(inter_munis)


dt <- data.frame(country="Brazil", stateProvince="São Paulo", locality=ucs$Nome.da.UC)