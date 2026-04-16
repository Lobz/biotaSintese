# Download JABOT data -----------------------------------------------------
# Author: guilherme gritz
# https://ipt.jbrj.gov.br/jabot/resource?r= is the base url.

#' Download IPT Resource
#'
#' @author Guilherme Gritz
#' @author Mali Oz Salles
#'
#' @param resource Name of the resource (ie, collection code)
#' @param baseUrl Start of the URL, lacking the resouce name
#' @param directory Destination folder
#' @param filename Destination file
downloadIPTResource <- function(resource,
                                  baseUrl = "https://ipt.jbrj.gov.br/jabot/resource?r=",
                                  directory = here::here("data-tmp", "JABOT"),
                                  filename = paste0(resource, ".zip")) {
    if(!dir.exists(directory))
        dir.create(directory)

    cat("Downloading", resource, "\n")

    # Get JABOT IPT url from each herbarium
    url <- paste0(baseUrl, resource)

    # Get its html content
    html_content <- rvest::read_html(url, encoding = "ISO-8859-1")

    # Find the node containing the most updated version (SelectorGadget addon is useful here)
    node <- rvest::html_node(html_content, css = "td a")

    # Get href attribute
    link <- rvest::html_attr(node, "href")

    # Download and save
    fullname <- file.path(directory, filename)
    download.file(url = link, destfile = fullname, mode = "wb")
}

openIPTresource <- function(file) {
    f <- utils::unzip(file, exdir = "data-tmp", files = "occurrence.txt", overwrite = T)
    dt <- data.table::fread(f, colClasses = "character")
    dt
}

downloadJabot <- function() {
    # Load the list of herbaria available in JABOT
    file_path <- here::here("data")
    file_name <- "herbaria_reflora.csv"
    herbaria <- data.table::fread(file.path(file_path, file_name))
    herbaria <- as.data.frame(herbaria)
    herbaria <- herbaria[order(herbaria$herbarium), ]

    lapply(herbaria, downloadIPTResource)
}

downloadReflora <- function() {
    lapply(herbariaReflora, downloadIPTResource,
        baseUrl = "https://ipt.jbrj.gov.br/reflora/resource?r=",
        directory = here::here("data-tmp", "Reflora"))
}
