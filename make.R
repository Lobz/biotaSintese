# Please read the README before running this

# Format data from each source
source("analyses/formatData/GBIF.R")
source("analyses/formatData/JABOT.R")
source("analyses/formatData/Reflora.R")
source("analyses/formatData/splink.R")

# Join data and format in Darwin core format
source("analyses/joinData.R")
# Treat data with plantR
source("analyses/saopaulo.R")
# Filter occs for each UC
source("analyses/getOccs.R")
# Generate checklists
source("analyses/treatOccs.R")
