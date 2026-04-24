
plantRWorkflow <- function(x) {
    # Standardize missing information
    x[x==""] <- NA

    # # Subset country
    # print(dim(x))
    # x <- subset(x, is.na(country) | grepl("br", tolower(country), fixed=T))

    # Lets format this
    print("Formatting occs")
    x <- formatOcc(x, noNumb = NA, noYear = NA, noName = NA)

    print("Formatting locs")
    x <- formatLoc(x)

    x <- fixLocation(x)

}