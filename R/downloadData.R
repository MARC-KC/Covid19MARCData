

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
downloadMARCCovidData <- function(dataset = c("CDT", "CDT_NewlyReported", "Hospital")) {

    #Match Arguments
    dataset <- match.arg(dataset)


    #Download Case, Death, Test Data
    if (dataset == "CDT") {
        message(crayon::yellow("Downloading Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    #Download the Newly Reported Case, Death, Test Data
    if (dataset == "CDT_NewlyReported") {
        message(crayon::yellow("Downloading Newly Reported Case, Death, and Test data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtestnewlyreported') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    #Download the Hospital Data
    if (dataset == "Hospital") {
        message(crayon::yellow("Downloading Hospital data from the MARC Data API: https://gis2.marc2.org/MARCDataAPI/api/covidhospital"))
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




#' @title Create Base Calculated Datasets from Raw Data
#'
#' @description  Create the Base Datasets that al of the COVID-19 Data Hub is ran from
#'
#' @param baseDataList A named list of data.frames containing the base data. See
#'   details for more information. Defaults to the return from
#'   \code{downloadAllCovidAPIData()}

#' @details \code{baseDataList} should contain a named list of the base data.frames. These
#'   are available through the MARC data API through the helpful functions
#'   \code{downloadMARCCovidData()} and \code{downloadAllCovidAPIData()} This should be 3
#'   data.frames with the following names:
#' \describe{
#'   \item{cdtData}{Case, Death, and Test Data}
#'   \item{cdtNRData}{Newly Reported Case, Death, and Test Data}
#'   \item{hospData}{Hospital Data}
#' }
#'
#' @return A list of data.frames that are used by MARC's COVID Data Hub.
#'
#' @export
getBaseCovidData <- function(baseDataList = downloadAllCovidAPIData()) {

    list2env(baseDataList, env = rlang::current_env())


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Combine Hospital and CDT Base Data ####
    message(crayon::blue("Combining CDT and Hospital Time Series"))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cdtHospData <- dplyr::full_join(cdtData, hospData, by = c("GeoID", "Date")) %>%
        marcR::coalesceJoin(showMessage = FALSE) %>%
        dplyr::mutate(CovidNew = CovidNew24HConfirmed + CovidNew24HSuspected) %>%
        dplyr::select(
            Jurisdiction, State, GeoID, Region, Date,

            CasesNew, CasesTotal,
            DeathsNew, DeathsTotal,
            TestsNew, TestsTotal,
            Population,

            HospitalsReporting, HospitalsTotal,
            BedsTotal, BedsUsed, BedsAvailable,
            BedsICUTotal, BedsICUUsed, BedsICUAvailable,
            CovidTotal,
            CovidNew,
            CovidICUTotal, CovidICUConfirmed, CovidICUSuspected,
            VentilatorsTotal, VentilatorsUsed, VentilatorsAvailable,
            CovidVentilatorsUsed)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculate Rolling Average Tables ####
    message(crayon::blue("Calculating 7 and 14 day rolling averages."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    varTable <- tibble::tribble(
        ~variable,                  ~Avg,         ~Total,     ~CalcString,
        "CasesNew",                 TRUE,         TRUE,       NA,
        "DeathsNew",                TRUE,         TRUE,       NA,
        "TestsNew",                 TRUE,         TRUE,       NA,
        "CovidNew",                 TRUE,         TRUE,       NA,
        "CovidTotal",               TRUE,         FALSE,      NA,
        "BedsUsedOther",            TRUE,         FALSE,      "BedsUsed - CovidTotal",
        "BedsAvailable",            TRUE,         FALSE,      NA,
        "CovidICUTotal",            TRUE,         FALSE,      NA,
        "BedsICUUsedOther",         TRUE,         FALSE,      "BedsICUUsed - CovidICUTotal",
        "BedsICUAvailable",         TRUE,         FALSE,      NA,
        "CovidVentilatorsUsed",     TRUE,         FALSE,      NA,
        "VentilatorsUsedOther",     TRUE,         FALSE,      "VentilatorsUsed - CovidVentilatorsUsed",
        "VentilatorsAvailable",     TRUE,         FALSE,      NA,
        "HospitalsReporting",       TRUE,         TRUE,       NA,
        "HospitalsTotal",           TRUE,         TRUE,       NA
    )


    cdtHosp7DayRollingData <- rollSummaryXDays(df = cdtHospData, numDays = 7, varTable = varTable)

    cdtHosp14DayRollingData <- rollSummaryXDays(df = cdtHospData, numDays = 14, varTable = varTable)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    outSummaries <- list('cdtHospData' = cdtHospData, 'cdtHosp7DayRollingData' = cdtHosp7DayRollingData, 'cdtHosp14DayRollingData' = cdtHosp14DayRollingData)

    out <- c(baseDataList, outSummaries)

    return(out)

}












