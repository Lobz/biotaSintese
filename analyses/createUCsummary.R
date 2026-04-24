require(integraFlora)
folder <- "data-input/Locations/info/"
info_files <- read.csv("data-input/Locations/info/file_descriptions.csv", na.strings = c("", "NA"))
filenames <- paste0("data-input/Locations/info/", info_files$arquivo)

# apply default values
info_files$sep[is.na(info_files$sep)] <- ","
info_files$dec[is.na(info_files$dec)] <- "."

# Acessory function to read data.frames
column_or_value <- function(x, dt) {
    if(x %in% names(dt)) {
        return(dt[,x])
    } else {
        x
    }
}

# Read data according to file descriptions
info_data_list <- lapply(1:nrow(info_files), function(i) {
    tryCatch({
        print(paste("Reading ", filenames[i], "..."))
        dt_raw <- as.data.frame(data.table::fread(filenames[i], sep = info_files$sep[i], dec = info_files$dec[i], na.strings = c("", "NA")))
        dt <- data.frame(
            name = standardize_uc_name(column_or_value(info_files$nome_uc[i], dt_raw)),
            country = column_or_value(info_files$pais[i], dt_raw),
            stateProvince = column_or_value(info_files$estadoProvincia[i], dt_raw),
            municipality = column_or_value(info_files$municipio[i], dt_raw),
            protectionCategory = column_or_value(info_files$categoria[i], dt_raw),
            UC_ID = column_or_value(info_files$codigo[i], dt_raw),
            source = info_files$arquivo[i]
        )
        if(anyDuplicated(dt$name)) {
            warning(paste(c("Found UCs with duplicated names:", shorten_uc_name(dt$name[duplicated(dt$name)])), collapse="\n"))
            dt <- dt[!duplicated(dt$name) & !is.na(dt$name),]
        }
        return(dt)
    },
    error = function(e) {
        print("Error reading file: ")
        warning(e)
        NULL
    })
})

lapply(info_data_list, head)

# Figure out which names are the same
merge_info <- function(A, B) {
    # Detect repeated values
    repA <- A$name %in% B$name
    repB <- B$name %in% A$name
    # add selected elements from first source to result
    r <- A[repA,]
    # remove repeated entries
    A <- A[!repA,]
    B <- B[!repB,]
    # detect similar entries
    strA <- paste0("^",generate_uc_string(A$name),"$")
    matchA <- sapply(strA, function(s) {any(grepl(s, x=rmLatin(B$name), ignore.case=T))})
    if(any(matchA)) {
        # add positively matched to result
        r <- rbind(r, A[matchA,])
        # remove those from further testing
        A <- A[!matchA,]

        # remove repeated from the other set
        whichA <- sapply(strA[matchA], grep, x=rmLatin(B$name), ignore.case=T)
        B <- B[-whichA,]
    }
    # detect similar entries (converse)
    strB <- paste0("^",generate_uc_string(B$name),"$")
    matchB <- sapply(strB, function(s) {any(grepl(s, x=rmLatin(A$name), ignore.case=T))})
    if(any(matchB)) {
        # add positively matched to result
        r <- rbind(r, B[matchB,])
        # remove those from further testing
        B <- B[!matchB,]

        # remove repeated from the other set
        whichB <- sapply(strB[matchB], grep, x=rmLatin(A$name), ignore.case=T)
        A <- A[-whichB,]
    }
    r <- rbind(r, A, B)
    return(r)
}

print("Merging info...")
dt <- info_data_list[[1]]
for(i in 2:length(info_data_list)) {
    print(paste("Merging", filenames[i], "..."))
    dt <- merge_info(dt, info_data_list[[i]])
}
nrow(dt)
tab(dt$source)

dt <- dt[order(dt$name),]
write.csv(dt, "data-input/Locations/info/Summary.csv", row.names=F)
