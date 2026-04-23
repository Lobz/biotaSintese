require(integraFlora)
require(plantR)

# Jabot data
jabot_files <- list.files("data-input/Occurrences/JABOT", pattern = "*.csv", full.names = TRUE)
if(length(jabot_files) > 0) {
    jabot_data_raw <- lapply(jabot_files, readJabot)
    jabot <- lapply(jabot_data_raw, formatJabot)
} else {
    jabot <- list()
}

save(jabot,file="data-tmp/jabot.RData")
