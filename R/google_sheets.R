## Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")


#' Download a GoogleSheets file
#'
#' @description
#' This function downloads n sheets from a GoogleSheets file as a list
#'
#' @param link the url of the GoogleSheets file
#' @param sheets the names of sheets to download
#' @param n the number of sheets to download (use only if sheets is unavailable)
#'
#' @return list of data.frame
#'
#' @examples
#' data <- download_sheets(DICTIONARIES_URL, 5)
#' @export
download_sheets <- function(link, sheets=NULL, n=1, ...) {

  ## Authenticate
  googlesheets4::gs4_auth()

  if(is.character(sheets)) {
    mysheets <- lapply(sheets, function(i) googlesheets4::read_sheet(link, range=i, na = c("NA", ""), col_types="c", ...))
    names(mysheets) <- sheets
  }
  else {
    ## Download n sheets
    mysheets <- lapply(1:n, function(i) googlesheets4::read_sheet(link, i, na = c("NA", ""), col_types="c", ...))
  }

  lapply(mysheets, as.data.frame)
}
