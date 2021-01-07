

#' @title Download MARC's COVID-19 Data
#'
#' @description Allows data to be downloaded from the MARC Data API with a
#'   simple function.
#'
#' @param dataset What dataset should be downloaded? Select one of 'CDT', 'CDT_NewlyReported', or 'Hospital'.
#'
#' @return A tibble with the selected COVID-19 data
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
downloadMARCCovidData<- function(dataset = c("CDT", "CDT_NewlyReported", "Hospital")) {

    #Match Arguments
    dataset <- match.arg(dataset)


    #Download Case, Death, Test Data
    if (dataset == "CDT") {
        message(crayon::yellow("Downloading Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest\n"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    #Download the Newly Reported Case, Death, Test Data
    if (dataset == "CDT_NewlyReported") {
        message(crayon::yellow("Downloading Newly Reported Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported\n"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    #Download the Hospital Data
    if (dataset == "Hospital") {
        message(crayon::yellow("Downloading Hospital data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidhospital\n"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidhospital') %>%
            dplyr::mutate(Date = as.Date(Date))
    }




    return(out)

}


#' @title Download All of MARC's COVID-19 Data
#'
#' @description Allows all the COVID-19 data to be downloaded from the MARC Data
#'   API with a simple function. This is a helper function for the data pipeline
#'   so that for testing purposes, data can be ingested directly from MARC
#'   internal SQL servers and not hard coded to the API data.
#'
#'
#' @return A list of tibbles containing the COVID-19 data
#'
#'
#' @examples
#' \dontrun{
#'
#' downloadAllCovidAPIData()
#'
#' }
#' @export
downloadAllCovidAPIData <- function() {

    cdtData <- downloadMARCCovidData(dataset = "CDT") %>%
        dplyr::filter(GeoID %in% GeoIDs[['base']])

    cdtNRData <-  downloadMARCCovidData(dataset = "CDT_NewlyReported") %>%
        dplyr::filter(GeoID %in% GeoIDs[['base']])

    hospData <-  downloadMARCCovidData(dataset = "Hospital") %>%
        dplyr::filter(GeoID %in% GeoIDs[['base']])

    out <- list('cdtData' = cdtData, 'cdtNRData' = cdtNRData, 'hospData' = hospData)

    return(out)
}

















