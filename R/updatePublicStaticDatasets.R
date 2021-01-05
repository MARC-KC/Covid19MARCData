
#' @title Check/Update Package Datasets
#'
#' @description Helper functions to check the dataset source
#'   (Covid10MARCInternal) for package dataset updates.
#'
#' @details `checkPublicStaticDatasets()` is called on package attach but is
#'   only ran if Covid19MARCInternal is installed.
#'   `updatePublicStaticDatasets()` is a helper that will do the dataset update
#'   if changes are detected
#'
#' @noRd
checkPublicStaticDatasets <- function() {

    if (requireNamespace("Covid19MARCInternal", quietly = TRUE)) {

        jurisTableInternal <- dplyr::select(Covid19MARCInternal::jurisdictionTable, Jurisdiction, Region, State, GeoID)
        popTableInternal <- dplyr::select(Covid19MARCInternal::jurisdictionTable, GeoID, Population)
        GeoIDsInternal <- Covid19MARCInternal::GeoIDs

        # if any not identical to package version provide startup message
        if (!identical(jurisTable, jurisTableInternal) | !identical(popTable, popTableInternal) | !identical(GeoIDs, GeoIDsInternal)) {
            packageStartupMessage('Data sets need updated. Open the Covid10MARCData repository, run devtools::load_all(), then run updatePublicStaticDatasets()')
        } else {
            # packageStartupMessage('1')
        }
    } else {
        # packageStartupMessage('2')
    }


}


#' @noRd
updatePublicStaticDatasets <- function() {

    if (requireNamespace("Covid19MARCInternal", quietly = TRUE)) {

        jurisTable <- dplyr::select(Covid19MARCInternal::jurisdictionTable, Jurisdiction, Region, State, GeoID)
        popTable <- dplyr::select(Covid19MARCInternal::jurisdictionTable, GeoID, Population)
        GeoIDs <- Covid19MARCInternal::GeoIDs


        if (!requireNamespace("usethis", quietly = TRUE)) {
            stop("Package \"usethis\" needed for this function to work. Please install it.",
                 call. = FALSE)
        }


        #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        # Export Data for Package Use ####
        #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        usethis::use_data(jurisTable,
                          popTable,
                          GeoIDs,
                          overwrite = TRUE)
        messsage('Make sure documentation for changes is up to date in /R/data.R, then document and rebuild package')
        #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    }


}
