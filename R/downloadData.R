

#' @title Download MARC's COVID-19 Data
#'
#' @description Allows data to be downloaded from the MARC Data API with a
#'   simple function.
#'
#' @param type What table should be downloaded? Select one of 'CDT', 'CDT_NewlyReported', or 'Hospital'.
#' @param source What should the source of the data be? Defaults to MARCDataAPI. Other options are passed to `Covid19MARCInternal::downloadBaseDataInternal()` for internal use.
#' @param date Default NULL. Only used by `Covid19MARCInternal::downloadBaseDataInternal()` for internal use.
#'
#' @details On the MARC internal network, the suggested Covid19MARCInternal
#'   package can be used to access the same data as provided through the API
#'   directly from the Publication and Production servers for testing purposes
#'
#' @return A tibble with the selected data
#'
#'
#' @examples
#' \dontrun{
#'
#' cdtData <- downloadBaseData(type = "CDT")
#'
#' cdtNRData <-  downloadBaseData(type = "CDT_NewlyReported")
#'
#'
#' }
#' @export
downloadBaseData <- function(type = c("CDT", "CDT_NewlyReported", "Hospital"), source = "MARCDataAPI", date = NULL) {

    #Match Arguments
    type <- match.arg(type)

    if (source == "MARCDataAPI") {

        #Download Case, Death, Test Data
        if (type == "CDT") {
            message(crayon::yellow("Downloading Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest\n"))
            out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>%
                dplyr::mutate(Date = as.Date(Date),
                              LastUpdated = lubridate::as_datetime(LastUpdated),
                              LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
        }

        #Download the Newly Reported Case, Death, Test Data
        if (type == "CDT_NewlyReported") {
            message(crayon::yellow("Downloading Newly Reported Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported\n"))
            out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported') %>%
                dplyr::mutate(Date = as.Date(Date),
                              LastUpdated = lubridate::as_datetime(LastUpdated),
                              LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
        }

        #Download the Hospital Data
        if (type == "Hospital") {
            message(crayon::yellow("Downloading Hospital data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidhospital\n"))
            out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidhospital') %>%
                dplyr::mutate(Date = as.Date(Date))
        }



    } else {


        if (!requireNamespace("Covid19MARCInternal", quietly = TRUE)) {
            stop("Package \"Covid19MARCInternal\" needed for this function to work. This is a private package that can only be used on the MARC internal network.",
                 call. = FALSE)
        }


        # out <- downloadBaseDataInternal(type = type, source = source, date = date)

    }

    return(out)

}


