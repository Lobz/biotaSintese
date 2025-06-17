library(plantR)
devtools::load_all()

# this will get all results from são paulo
url_col <- 'https://specieslink.net/ws/1.0/participants?apikey=qUe5HQpZDZH3yFNKnjMj'

data_col <- jsonlite::fromJSON(url_col)

flatten_list <- function(x, parent = "") {
    codes <- names(x)
    types <- sapply(x, function(x) x$type)
    names <- sapply(x, function(x) x$name)
    ids <- sapply(x, function(x) x$id)

    df <- data.frame(code = codes, name = names, type = types, id = ids, parent = parent)

    dfs <- lapply(x, function(x) {
        if(!is.null(x$nodes))
            flatten_list(x$nodes, parent = x$id)
    })

    dfs <- dplyr::bind_rows(dfs)
    rbind(df, dfs)
}

df <- flatten_list(data_col)
df_col <- subset(df, type=="col")

expected <- read.csv("data/raw-data/spl_sp_resumo_collections.csv")
expected$code <- stringr::str_squish(expected$code)

expected <- merge(expected, df_col)

sofar <- as.data.frame(table(splsaopaulo$collectionid))
names(sofar) <- c("id", "num_records")

sofar <- merge(expected, sofar)
summary(sofar$records-sofar$num_records)
sofar$id[(sofar$records==sofar$num_records)]

togo <- subset(sofar, records > num_records)
ids <- togo$id

splsaopaulo <- subset(splsaopaulo, !collectionid %in% ids)

head(togo)

splinkkey <- 'qUe5HQpZDZH3yFNKnjMj'
for(i in 2:nrow(togo)) {
    print(paste("Collection:", togo$code[i]))
    print(paste("Expected records:", togo$records[i]))
    id <- togo$id[i]
    one_batch <- rspeciesLink(
        Scope = "p", # this should filter out animals, but unreliable in filtering out fungi, bacteria, etc
        stateProvince = "Sao Paulo",
        collectionID = id,
        kingdom = "Plantae",
        key = splinkkey,
        MaxRecords = 5000)
    my_list <- list(one_batch)
    nrecords = 5000

    while(nrow(one_batch) == 5000) {
        one_batch <- rspeciesLink(
            Scope = "p", # this should filter out animals, but unreliable in filtering out fungi, bacteria, etc
            stateProvince = "Sao Paulo",
            collectionID = id,
            key = splinkkey,
            kingdom = "Plantae",
            MaxRecords = 5000,
            offset = as.integer(nrecords))
        my_list[[length(my_list) + 1]] <- one_batch
        nrecords <- nrecords+5000
        print(nrecords)
    }

    bound <- dplyr::bind_rows(my_list)
    print(nrow(bound))
    splsaopaulo <- dplyr::bind_rows(splsaopaulo, bound)
    print(nrow(splsaopaulo))
    splsaopaulo <- subset(splsaopaulo, kingdom == "Plantae")
    splsaopaulo$downloadedFrom <- "SPLINK"

    print(nrow(splsaopaulo))
    save(splsaopaulo, file="data/raw-data/spl_saopaulo.RData")
}

    # Merge and treat data
spl <- formatDwc(
    splink_data = splsaopaulo
    )

# Remove a problem case that anyway isn't very useful
spl <- subset(spl, barcode != "IPA0014822" | is.na(barcode))
# todo: decide what to do with barcode NA

spl <- formatOcc(spl)
spl <- formatLoc(spl)
save(spl,file="data/derived-data/occs_splink_sp.RData")
