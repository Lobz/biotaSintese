devtools::load_all()

# Jabot data
jabot_files <- list.files("data-input/Occurrences/JABOT", pattern = "*.csv", full.names = TRUE)
jabot_data_raw <- lapply(jabot_files, read.csv, sep="|", na.strings = c("", "NA"))
jabot <- formatJabot(jabot_data_raw[[1]])
if(length(jabot_data_raw) > 1) {
    print("Merging JABOT databases...")
    for(i in 2:length(jabot_data_raw)){
        #merge
        jabot <- merge(jabot, formatJabot(jabot_data_raw[[i]]), all=T)
    }
}

print(paste("Found",nrow(jabot), "observations."))

save(jabot,file="data-tmp/jabot.RData")
