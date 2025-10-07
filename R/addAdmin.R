
addAdmin <- function(x) {

    locs <- getAdmin(x$loc.correct)
    names(locs)<-c("loc.correct.admin", "country.correct", "stateProvince.correct", "municipality.correct", "locality.correct", "source.loc")
    x[,names(locs)] <- NULL
    x <- cbind(x,locs)
    x
}
