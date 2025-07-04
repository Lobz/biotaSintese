library(plantR) # used foi reading and cleaning occurrence data
devtools::load_all()

# Read list from Catalogo das Plantas das UCs do Brasil
cl0 <- read.csv(("data/raw-data/Dados_Catalogo_UCs_Brasil.csv"))
names(cl0)
cl0$scientificName <- substr(cl0$Táxon, nchar(cl0$Família) + 2, nchar(cl0$Táxon))
cl0 <- formatTax(cl0)
dim(cl0)
familiesRef <- unique(cl0$family.new)
length(familiesRef)
speciesRef <- unique(cl0$scientificName.new)
length(speciesRef)
str(cl0)
catalogoCompleto <- get_species_and_genus(cl0)

save(catalogoCompleto, file="data/raw-data/catalogoCompleto.RData")
