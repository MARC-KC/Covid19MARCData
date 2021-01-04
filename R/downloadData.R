
downloadBaseData <- function(type = c("CDT", "CDT_NewlyReported", "Hospital"), source = "MARCDataAPI", date = NULL) {

    #Match Arguments
    type <- match.arg(type)

    if (source == "MARCDataAPI") {

        #Download Case, Death, Test Data
        if (type == "CDT") {
            cat(crayon::yellow("Downloading Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest\n"))
            out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>%
                dplyr::mutate(Date = as.Date(Date),
                              LastUpdated = lubridate::as_datetime(LastUpdated),
                              LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
        }

        #Download the Newly Reported Case, Death, Test Data
        if (type == "CDT_NewlyReported") {
            cat(crayon::yellow("Downloading Newly Reported Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestNewlyReported\n"))
            out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestNewlyReported') %>%
                dplyr::mutate(Date = as.Date(Date),
                              LastUpdated = lubridate::as_datetime(LastUpdated),
                              LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
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


