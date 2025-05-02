library(plantR)

# Jabot data
jabot_raw <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot_raw$county <- NA
# fix names
my_names_dwc <- names(reflora_gbif)
matchtab <- data.frame(my_names_dwc, tolower(my_names_dwc))
names_jabot <- names(jabot_raw)
matched <- match(names_jabot, matchtab[,2])
has_match <- !is.na(matched)
matched <- matched[has_match]
names(jabot_raw)[has_match] <- matchtab[matched,1]

jabot_raw$downloadedFrom <- "JABOT"

jabot <- formatDwc(user_data=jabot_raw)

jabot <- formatOcc(jabot)
jabot <- formatLoc(jabot)

save(jabot,file="data/derived-data/jabot_saopaulo_dwc.RData")
