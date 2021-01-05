
#' On attach, if on a MARC computer with Covid19MARCInternal installed, check for updates to datasets. If there are updates print a message
#' @noRd
.onAttach <- function(...) {
    checkPublicStaticDatasets()
}
