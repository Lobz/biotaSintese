# Jabot data
jabot <- read.csv("../../BIOTA/JABOT/JABOT_SaoPaulo_DarwinCore.csv", sep="|", na.strings=c("","NA"))
jabot$county <- NA

save(jabot,file="data/derived-data/jabot_saopaulo.RData")
