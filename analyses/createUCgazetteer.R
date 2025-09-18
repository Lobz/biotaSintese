devtools::load_all()
library(plantR) # used for reading and cleaning occurrence data
library(sf)
library(geobr)

# We should generate a gazeteer for the UCs

# Whenever possible, we want to extract coordinates from the cnuc data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes$nome_uc_standard <- standardize_uc_name(shapes$nome_uc)

# We will NOT be trusting the cnuc text data for what municipalities are included in each UC
# Instead, we will use the shape data and compare it with geobr data
munis <- geobr::read_municipality(year = 2024)
states <- geobr::read_state(year = 2020)




dt <- data.frame(country="Brazil", stateProvince="São Paulo", locality=ucs$Nome.da.UC)