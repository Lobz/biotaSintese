# Load gazetteer, taxonomists from raw-dictionaries, and phylo data
path_plantR <- file.path("..", "plantR", "data-raw", "raw_dictionaries", "taxonomists.csv")
tax_plantR <- read.csv(path_plantR)
fam_plantR <- plantR:::familiesSynonyms


# GBIF specialists by number of registers identified ----------------------

# Loop through each family and get a list of taxonomists from our records
# Keep only taxonomists that identified more than 100 individuals in the family
missing_families <- c()
tax_list <- list()

# Loop through family names
for (i in family_names) {
  rds_file <- file.path(file_path, i, paste0(i, ".rds"))

  if (file.exists(rds_file)) {
    family_data <- readRDS(rds_file)
    family_data <- family_data[, c("tax.check1", "identifiedBy.new1", "basisOfRecord")]
  } else {
    cat("File not found:", i, "\n")
    missing_families <- c(missing_families, i)
    next
  }

  if (all(family_data$tax.check1 == "high")) {
    next
  } else {
    non.tax.det <- family_data[family_data$basisOfRecord %in% c("PRESERVED_SPECIMEN",
                                                                "PreservedSpecimen"), ]
    non.tax.det <- non.tax.det[, 1:2]
    non.tax.det <- sort(table(family_data[family_data$tax.check1 %in% "low", "identifiedBy.new1"]))
    non.tax.det.df <- data.frame(names(non.tax.det), as.double(non.tax.det))
    # non.tax.det.df$prop.det <- non.tax.det.df$as.double.non.tax.det. / dim(family_data)[1]
    row.names(non.tax.det.df) <- NULL
    colnames(non.tax.det.df) <- c("tdwg.name", "family1")
    # 176 was the median of identifications considering a minimum number of 100 IDs for families
    # Using 150
    non.tax.det.df <- non.tax.det.df[non.tax.det.df$family1 >= 150, ]
    non.tax.det.df <- non.tax.det.df[order(non.tax.det.df$family1, decreasing = TRUE), ]
    non.tax.det.df <- non.tax.det.df[!non.tax.det.df$tdwg.name %in% "s.n", ]

    if (nrow(non.tax.det.df) == 0){
      next
    } else {

      # Populate new columns
      non.tax.det.df$family <- i
      non.tax.det.df$source <- "gbif_GSG"
      tax_list[[i]] <- non.tax.det.df
    }
  }
}

# This tax_list is the first new input to plantR taxonomists
tax_list <- do.call(rbind, tax_list)
rownames(tax_list) <- NULL
tax_list$full.name1 <- NA
tax_list$last.name <- tolower(plantR::lastName(tax_list$tdwg.name))
tax_list$family.obs <- NA
tax_list$order <- NA
tax_list$family.obs1 <- NA



# Scrapping COL ChecklistBank ---------------------------------------------
# Data will be scrapped from https://api.checklistbank.org/dataset/2004.yaml
file_path1 <- here::here("data", "raw-data")
file_name1 <- "wfo_ChecklistBank_taxonomists.yaml"
parsed <- yaml::read_yaml(file.path(file_path1, file_name1))

# Only necessary columns
taxonomists <- lapply(parsed$editor, function(x) {
  list(given = x$given,
       surname = x$family,
       note = x$note)})
# Bind it all
taxonomists <- do.call(rbind, lapply(taxonomists, as.data.frame))

# Minor edits
taxonomists$note <- gsub("Curator of ", "", taxonomists$note)
taxonomists$note <- gsub(" in the Rhakhis editor.", "", taxonomists$note)
taxonomists$note <- gsub(" and", ",", taxonomists$note)
taxonomists$note <- gsub("Gaultheria Diplycosia", "Gaultheria, Diplycosia", taxonomists$note)

# Removing Govaerts to add it later with its specific families as specialist
taxonomists <- taxonomists[!(taxonomists$surname %in% "Govaerts"), ]

# Scrap all genus that belong to each subfamily from https://www.legumedata.org/

# Fabaceae subfamilies
fab_subfam <- c("caesalpinioideae", "cercidoideae", "detarioideae",
                "dialioideae", "duparquetioideae", "papilionoideae")
fab_subfam_grep <- paste0(gsub("^(.)", "\\U\\1", fab_subfam, perl = TRUE), collapse = "|")

# Initialize an empty list to store data frames
fabaceae_list <- list()

# Base url
base_url <- "https://www.legumedata.org/taxonomy/"

# Loop through each subfamily
for (i in fab_subfam) {
  url <- paste0(base_url, i)
  html <- rvest::read_html(url, encoding = "ISO-8859-1")
  html_nodes <- rvest::html_nodes(html, css = "tr td:nth-child(1)")
  html_text <- rvest::html_text(html_nodes)
  html_text <- gsub("\\*", "", html_text)

  df <- data.frame(subfamily = i,
                   genus = html_text,
                   stringsAsFactors = FALSE)

  fabaceae_list[[i]] <- df
}

fabaceae_list <- do.call(rbind, lapply(fabaceae_list, as.data.frame))
rownames(fabaceae_list) <- NULL
fabaceae_list$subfamily <- gsub("^(.)", "\\U\\1", fabaceae_list$subfamily, perl = TRUE)
fabaceae_list$genus_clean <- gsub("[[:space:]].*", "", fabaceae_list$genus, perl = TRUE)

# Removing a name from a weird css path
fabaceae_list <- fabaceae_list[!grepl("_", fabaceae_list$genus), ]

# Aggregate to add the info to the taxonomists
fabaceae_list_agg <- aggregate(genus ~ subfamily,
                               function(x) paste0(unique(x), collapse = ", "),
                               data = fabaceae_list)

notes_splitted <- strsplit(taxonomists$note, split = ", ")

# Get the Fabaceae people
get_these <- which(grepl(fab_subfam_grep, taxonomists$note))

# Populate new columns
taxonomists$family <- taxonomists$note

# gsub Fabaceae subfamilies in column family by Fabaceae
for(i in seq_along(fabaceae_list_agg$subfamily)){
  subfamily <- fabaceae_list_agg$subfamily[i]
  taxonomists$family[get_these] <- gsub(subfamily, "Fabaceae", taxonomists$family[get_these])
}

# gsub some orders by their families
# Commelinales, Cycadales, Dipsacales, Caryophyllales
# Only angiosperms since we are using APG data
manual_fam <- unique(fam_plantR[fam_plantR$order.correct %in%
                                  c("Commelinales", "Cycadales",
                                    "Dipsacales", "Caryophyllales") ,
                                c("order.correct", "name.correct")])
manual_fam_agg <- aggregate(name.correct ~ order.correct,
                            FUN = function(x) paste0(sort(x), collapse = ", "),
                            data = manual_fam)

for (i in 1:nrow(manual_fam_agg)) {
  order_correct <- manual_fam_agg$order.correct[i]
  name_correct <- manual_fam_agg$name.correct[i]
  taxonomists$family <- gsub(order_correct, name_correct, taxonomists$family)
}

# Move non-families to family.obs; keeping only families in family col
taxonomists$family.obs <- NA
taxonomists$family.obs[get_these] <- taxonomists$note[get_these]

family_split <- strsplit(taxonomists$family, ", ")

# Initialize vectors to store the results
family_new <- vector("list", length(family_split))
family_obs_new <- vector("list", length(family_split))

for (i in seq_along(family_split)) {
  # Keep only families here
  aceae_elements <- family_split[[i]][grepl("aceae$", family_split[[i]])]
  family_new[[i]] <- if(length(aceae_elements) == 0) NA else paste(aceae_elements, collapse = ", ")

  # Move other stuff
  non_aceae_elements <- family_split[[i]][!grepl("aceae$", family_split[[i]])]
  family_obs_new[[i]] <- if(length(non_aceae_elements) == 0) NA else paste(non_aceae_elements, collapse = ", ")
}

# Update
taxonomists$family <- unlist(family_new)
taxonomists$family.obs <- ifelse(is.na(taxonomists$family.obs), unlist(family_obs_new),
                                 paste(taxonomists$family.obs, unlist(family_obs_new), sep = ", "))
taxonomists$family.obs <- gsub(", NA", "", taxonomists$family.obs)

# If anything duplicated in these columns, simplify
any_dup <- strsplit(taxonomists$family, split = ", ")
taxonomists$family <- unlist(lapply(any_dup, function(x) paste((unique(x[!is.na(x)])), collapse = ", ")))
any_dup <- strsplit(taxonomists$family.obs, split = ", ")
taxonomists$family.obs <- unlist(lapply(any_dup, function(x) paste((unique(x[!is.na(x)])), collapse = ", ")))

# Remove any families from family.obs
any_fam <- strsplit(taxonomists$family.obs, split = ", ")
taxonomists$family.obs <- unlist(lapply(
  strsplit(taxonomists$family.obs, split = ", "), function(x) {
    filtered_elements <- x[!grepl("aceae$", x)]
    if (length(filtered_elements) == 0) {
      NA
    } else {
      paste(unique(filtered_elements), collapse = ", ")
    }
  }
))

# Just populate NAs where is empty
taxonomists[taxonomists == ""] <-NA

# Get the family from the genus in family.obs
get_genus <- na.omit(unlist(strsplit(taxonomists$family.obs, split = ", ")))
# Remove orders and subfamilies...
get_genus <- get_genus[!grepl("ales$|oideae$", get_genus, perl = TRUE)]
# Species specialists...
get_genus <- gsub("[[:space:]].*", "", get_genus)
# Match against WFO
get_genus1 <- plantR::prepSpecies(get_genus, db = plantRdata::wfoNames,
                                  use.authors = FALSE, mult.matches = 'best',
                                  sug.dist = 0.95, drop.cols = NULL)
get_genus1 <- get_genus1[!duplicated(get_genus1), ]
get_genus1 <- get_genus1[!is.na(get_genus1$suggestedName), c("family", "suggestedName")]
get_genus1 <- get_genus1[!get_genus1$family %in% c("", NA), ]

# Substitute each genus by its respective family
taxonomists$family.obs1 <- NA

for (i in 1:nrow(taxonomists)) {
  generos <- strsplit(taxonomists$family.obs[i], ",\\s*")[[1]]
  resultados <- generos
  for (j in 1:length(generos)) {
    gen_correct <- generos[j]

    if (gen_correct %in% get_genus1$suggestedName) {
      fam_correct <- get_genus1$family[get_genus1$suggestedName == gen_correct]
      resultados[j] <- fam_correct
    }
  }
  taxonomists$family.obs1[i] <- paste(resultados, collapse = ", ")
}

# Again, remove duplicates from family.obs1
any_dup <- strsplit(taxonomists$family.obs1, split = ", ")
taxonomists$family.obs1 <- unlist(lapply(any_dup, function(x) paste((unique(x[!is.na(x)])), collapse = ", ")))
taxonomists$family.obs1 <- gsub("NA", NA, taxonomists$family.obs1)

# Adding new columns following plantR standards
taxonomists$order <- NA
taxonomists$source <- "wfoChecklistBank"
taxonomists$full.name1 <- paste(taxonomists$given, taxonomists$surname)
taxonomists$tdwg.name <- plantR::prepName(taxonomists$full.name1)
taxonomists$family1 <- NA
taxonomists$last.name <- plantR::lastName(taxonomists$tdwg.name)

# Split authors by families
taxonomists_expanded <- tidyr::separate_rows(taxonomists, family, sep = ",\\s*")

write.csv(taxonomists_expanded, "data/taxonomists_expanded.csv")


# Extracting experts from POWO reviewers ----------------------------------

# Base url
url <- "https://powo.science.kew.org/compilers-and-reviewers"
html <- rvest::read_html(url)
html_nodes <- rvest::html_node(html, css = ".table :nth-child(1)")
html_table <- rvest::html_table(html_nodes)

# Get only families with reviewers
html_table <- html_table[!html_table$`Reviewer(s)` %in% c("", NA), ]
html_table$`Reviewer(s)` <- gsub("\\([^()]+\\)", "", html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- trimws(html_table$`Reviewer(s)`)

# Split each family by its reviewer
html_table <- tidyr::separate_rows(html_table, `Reviewer(s)`, sep = ", ")

# Minor text edits
html_table$`Reviewer(s)` <- gsub("\\.$", "", html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- gsub(" \\(SPF$", "", html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- gsub("[0-9]", "", html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- gsub("  For more information on the family and especially specimen information",
                                 "",
                                 html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- gsub("please visit the Sapotaceae Resource Centre",
                                 "",
                                 html_table$`Reviewer(s)`)
html_table$`Reviewer(s)` <- gsub("For reviewers please see GBIF",
                                 "",
                                 html_table$`Reviewer(s)`)
html_table <- html_table[!html_table$`Reviewer(s)` %in% c("", NA), ]

# Preparing the columns
html_table <- html_table[, c("Family", 'Reviewer(s)')]
names(html_table) <- c("family", "full.name1")
html_table$tdwg.name <- plantR::prepName(html_table$full.name1)
html_table$last.name <- tolower(plantR::lastName(html_table$tdwg.name))
html_table$order <- NA
html_table$source <- "powoReviewers"
html_table$family1 <- NA
html_table$family.obs <- NA
html_table$family.obs1 <- NA



# Extracting experts from CNCFlora GTA (2018) -----------------------------
# https://dspace.jbrj.gov.br/jspui/handle/doc/101
# Conservation Assessment of Brazilian tree species towards the Global Tree Assessment
cncflora <- readxl::read_xlsx(here::here("data", "raw-data", "cncflora_taxonomists.xlsx"))
cncflora$tdwg.name <- plantR::prepName(cncflora$taxonomist)

# Add other columns to merge
cncflora$source <- "cncflora_2018"
cncflora$order <- NA
cncflora$family.obs <- NA
cncflora$family.obs1 <- NA
names(cncflora)[names(cncflora) == "taxonomist"] <- "full.name1"
cncflora$last.name <- tolower(plantR::lastName(cncflora$tdwg.name))
cncflora$family1 <- NA



# Merge names from GBIF, WFO, and POWO ------------------------------------
to_cols <- c("order", "source", "family", "family.obs", "family.obs1",
             "full.name1", "tdwg.name", "last.name", "family1")

# GBIF
tax_list <- tax_list[, to_cols]
# wfoChecklistBank
taxonomists <- taxonomists_expanded[, to_cols]
taxonomists <- as.data.frame(taxonomists)
# POWO
html_table <- html_table[, to_cols]
# CNCFlora <- cncflora[, to_cols]
cncflora <- cncflora[, to_cols]

# Checking sequentially by order of importance of source: wfo, powo, cncflora, gbif
# Which names from the WFO ChecklistBank are not in plantR?
missing_tax <- setdiff(taxonomists$tdwg.name, tax_plantR$tdwg.name) # 134 taxonomists

# Bind these, but first edit the order column
tax_plantR_new <- dplyr::bind_rows(taxonomists[taxonomists$tdwg.name %in% missing_tax, ],
                                   tax_plantR)

no_order <- which(is.na(tax_plantR_new$order))
max_order <- max(tax_plantR_new$order, na.rm = TRUE)
tax_plantR_new$order[no_order] <- (max_order + 1):(max_order + length(no_order))

# How about POWO?
missing_tax <- setdiff(html_table$tdwg.name, tax_plantR_new$tdwg.name)
tax_plantR_new <- dplyr::bind_rows(html_table[html_table$tdwg.name %in% missing_tax, ],
                                   tax_plantR_new)

no_order <- which(is.na(tax_plantR_new$order))
max_order <- max(tax_plantR_new$order, na.rm = TRUE)
tax_plantR_new$order[no_order] <- (max_order + 1):(max_order + length(no_order))

# How about CNCFlora?
missing_tax <- setdiff(cncflora$tdwg.name, tax_plantR_new$tdwg.name)
tax_plantR_new <- dplyr::bind_rows(cncflora[cncflora$tdwg.name %in% missing_tax, ],
                                   tax_plantR_new)

no_order <- which(is.na(tax_plantR_new$order))
max_order <- max(tax_plantR_new$order, na.rm = TRUE)
tax_plantR_new$order[no_order] <- (max_order + 1):(max_order + length(no_order))

# Finally, how about GBIF?
missing_tax <- setdiff(tax_list$tdwg.name, tax_plantR_new$tdwg.name)

# Bind again
tax_list$family1 <- as.character(tax_list$family1)
tax_plantR_new <- dplyr::bind_rows(tax_list[tax_list$tdwg.name %in% missing_tax, ],
                                   tax_plantR_new)

no_order <- which(is.na(tax_plantR_new$order))
max_order <- max(tax_plantR_new$order, na.rm = TRUE)
tax_plantR_new$order[no_order] <- (max_order + 1):(max_order + length(no_order))

# Before saving, add a "?" after the family name to indicate that that taxonomists
# was not manually checked as a family taxonomists
get_these <- which(tax_plantR_new$source %in% "gbif_GSG")
tax_plantR_new$family[get_these] <- paste0(tax_plantR_new$family[get_these], "?")

# Save
path_save <- here::here("data", "derived-data", "raw_dic_taxonomists.rds")
saveRDS(tax_plantR_new, path_save)
