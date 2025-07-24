devtools::load_all()
library(stringr)
library(parallel)
library(sf)

# Shape data
shapes <- st_read("data/raw-data/shp_cnuc_2025_03/cnuc_2025_03.shp")
shapes <- subset(shapes, uf == "SÃO PAULO")
shapes$nome_uc <- standardize_uc_name(shapes$nome_uc)
shapes <- shapes[order(shapes$nome_uc), ]
shapes_buff <- st_buffer(shapes, 100)

# Intersect shapes with shapes
shapes_intersect <- st_intersects(shapes)
shapes_covers <- st_covers(shapes)
shapes_covers_buffer <- st_covers(shapes_buff, shapes)
inters <- st_intersects(shapes)

plot(st_geometry(shapes[107,]))
plot(st_geometry(shapes[shapes_covers_buffer[[107]][-1],]), add=T, col=shapes_covers_buffer[[107]])

coverage <- lapply(1:nrow(shapes), function(i) {
    name <- shapes$nome_uc[i]
    covered <- shapes$nome_uc[shapes_covers[[i]][-1]]
    covered_buff <- shapes$nome_uc[shapes_covers_buff[[i]][-1]]
    covered_buff <- setdiff((covered_buff, covered))
    inters <- shapes$nome_uc[shapes_intersect[[i]][-1]]
    inters <- setdiff(inters, covered)

})
