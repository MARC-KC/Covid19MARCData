

#' @title Download MARC's COVID-19 Data
#'
#' @description Allows data to be downloaded from the MARC Data API with a
#'   simple function.
#'
#' @param dataset What dataset should be downloaded? Select one of 'CDT',
#'   'CDT_NewlyReported' 'Hospital', 'Vaccination', or 'VaccinationCDC'.
#'
#' @return A tibble with the selected COVID-19 data
#'
#'
#' @examples
#' \dontrun{
#'
#' cdtData <- downloadBaseData(dataset = "CDT")
#'
#' cdtNRData <-  downloadBaseData(dataset = "CDT_NewlyReported")
#'
#'
#' }
#' @export
downloadMARCCovidData <- function(dataset = c("CDT", "CDT_NewlyReported", "Hospital", "Vaccination", "VaccinationCDC")) {

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


    #Download the Vaccination Data
    if (dataset == "Vaccination") {
        message(crayon::yellow("Downloading Vaccination data from the MARC Data API: https://gis2.marc2.org/marcdataapi/api/covidvaccination"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/marcdataapi/api/covidvaccination') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    #Download the Vaccination Data
    if (dataset == "VaccinationCDC") {
        message(crayon::yellow("Downloading CDC Vaccination data from the MARC Data API: https://gis2.marc2.org/marcdataapi/api/covidvaccinationcdc"))
        out <- marcR::MARCDataAPI_read('https://gis2.marc2.org/marcdataapi/api/covidvaccinationcdc') %>%
            dplyr::mutate(Date = as.Date(Date),
                          LastUpdated = lubridate::as_datetime(LastUpdated),
                          LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
    }

    out
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
        dplyr::filter(GeoID %in% Covid19MARCData::GeoIDs[['base']])

    cdtNRData <-  downloadMARCCovidData(dataset = "CDT_NewlyReported") %>%
        dplyr::filter(GeoID %in% Covid19MARCData::GeoIDs[['base']])

    hospData <-  downloadMARCCovidData(dataset = "Hospital") %>%
        dplyr::filter(GeoID %in% Covid19MARCData::GeoIDs[['base']])

    vaccData <- downloadMARCCovidData(dataset = "Vaccination")  #don't cut to base GeoID because it filters out Missouri

    vaccCDCData <- downloadMARCCovidData(dataset = "VaccinationCDC")

    out <- list('cdtData' = cdtData, 'cdtNRData' = cdtNRData, 'hospData' = hospData,
                'vaccData' = vaccData, "vaccCDCData" = vaccCDCData)

    out
}




#' @title Create Base Calculated Datasets from Raw Data
#'
#' @description  Create the Base Datasets that all of the COVID-19 Data Hub is ran from
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
#'   \item{vaccData}{Vaccination Data}
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
        dplyr::mutate(
            BedsInpatientUsedCovid = CovidConfirmedAdult + CovidConfirmedPediatric
            # BedsInpatientUsedCovidProportion = BedsInpatientUsedCovid/dplyr::if_else(BedsInpatientTotal == 0, NA_integer_, BedsInpatientTotal)
        ) %>%
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
            CovidVentilatorsUsed,
            BedsInpatientUsedCovid, BedsInpatientTotal)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Full Hospital Data With Calculations And Most Recent ####
    message(crayon::blue("Formatting base hospital data."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    hospData <- hospData %>%
        dplyr::mutate(CovidNew = CovidNew24HConfirmed + CovidNew24HSuspected) %>%
        dplyr::mutate(
            #Calculate Inpatient Beds used by COVID
            BedsInpatientUsedCovid = CovidConfirmedAdult + CovidConfirmedPediatric,

            #Calculate Used by Other Columns
            BedsUsedOther = (BedsUsed - CovidTotal),
            BedsICUUsedOther = (BedsICUUsed - CovidICUTotal),
            VentilatorsUsedOther = (VentilatorsUsed - CovidVentilatorsUsed),
            #Force Non-Negative
            BedsUsedOther = dplyr::if_else(BedsUsedOther < 0, as.integer(0), BedsUsedOther),
            BedsICUUsedOther = dplyr::if_else(BedsICUUsedOther < 0, as.integer(0), BedsICUUsedOther),
            VentilatorsUsedOther = dplyr::if_else(VentilatorsUsedOther < 0, as.integer(0), VentilatorsUsedOther),


            #Explicitly Calculate Totals using used and available
            BedsTotal = (BedsAvailable + BedsUsedOther + CovidTotal),
            BedsICUTotal = (BedsICUAvailable + BedsICUUsedOther + CovidICUTotal),
            VentilatorsTotal = (VentilatorsAvailable + VentilatorsUsedOther + CovidVentilatorsUsed),

            #Calculate Proportions of Use
            BedsAvailableProportion = BedsAvailable/dplyr::if_else(BedsTotal == 0, NA_integer_, BedsTotal),
            BedsUsedOtherProportion = BedsUsedOther/dplyr::if_else(BedsTotal == 0, NA_integer_, BedsTotal),
            CovidTotalProportion = CovidTotal/dplyr::if_else(BedsTotal == 0, NA_integer_, BedsTotal),
            BedsICUAvailableProportion = BedsICUAvailable/dplyr::if_else(BedsICUTotal == 0, NA_integer_, BedsICUTotal),
            BedsICUUsedOtherProportion = BedsICUUsedOther/dplyr::if_else(BedsICUTotal == 0, NA_integer_, BedsICUTotal),
            CovidICUTotalProportion = CovidICUTotal/dplyr::if_else(BedsICUTotal == 0, NA_integer_, BedsICUTotal),
            VentilatorsAvailableProportion = VentilatorsAvailable/dplyr::if_else(VentilatorsTotal == 0, NA_integer_, VentilatorsTotal),
            VentilatorsUsedOtherProportion = VentilatorsUsedOther/dplyr::if_else(VentilatorsTotal == 0, NA_integer_, VentilatorsTotal),
            CovidVentilatorsUsedProportion = CovidVentilatorsUsed/dplyr::if_else(VentilatorsTotal == 0, NA_integer_, VentilatorsTotal),
            BedsInpatientUsedCovidProportion = BedsInpatientUsedCovid/dplyr::if_else(BedsInpatientTotal == 0, NA_integer_, BedsInpatientTotal)
        )

    #HospitalTotal based on a 3 week window so that it can adapt to reporting over time
    hospData <- hospData %>%
        dplyr::mutate(
            HospitalsTotal = purrr::map2_int(GeoID, Date, ~dplyr::filter(hospData, GeoID == .x & Date >= .y - 10 & Date <= .y + 10)[['HospitalsReporting']] %>% max(., na.rm = TRUE) %>% as.integer())
        )
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculate Vaccinations by Age Range Columns ####
    message(crayon::blue("Calculating Vaccinations by Age Range Columns."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    vaccCDCData <- vaccCDCData %>%
        dplyr::mutate(

            # Calculate Vaccines Completed 5-11
            RegimenCompleted_Total5to11 = RegimenCompleted_TotalGTE5 - RegimenCompleted_TotalGTE12,
            Population_5to11 = PopulationGTE5 - PopulationGTE12,
            RegimenCompleted_Total5to11Prop = (RegimenCompleted_Total5to11)/Population_5to11,
            NotFullyVaccinated_5to11 = Population_5to11 - RegimenCompleted_Total5to11, #Either initiated not completed, or not initiated
            NotFullyVaccinated_5to11Prop = NotFullyVaccinated_5to11/Population_5to11,

            # Calculate Vaccines Completed 12-17
            RegimenCompleted_Total12to17 = RegimenCompleted_TotalGTE12 - RegimenCompleted_TotalGTE18,
            Population_12to17 = PopulationGTE12 - PopulationGTE18,
            RegimenCompleted_Total12to17Prop = (RegimenCompleted_Total12to17)/Population_12to17,
            NotFullyVaccinated_12to17 = Population_12to17 - RegimenCompleted_Total12to17, #Either initiated not completed, or not initiated
            NotFullyVaccinated_12to17Prop = NotFullyVaccinated_12to17/Population_12to17,

            # Calculate Vaccines Completed 18 - 65
            RegimenCompleted_Total18to65 = RegimenCompleted_TotalGTE18 - RegimenCompleted_TotalGTE65,
            Population_18to65 = PopulationGTE18 - PopulationGTE65,
            RegimenCompleted_Total18to65Prop = (RegimenCompleted_Total18to65)/Population_18to65,
            NotFullyVaccinated_18to65 = Population_18to65 - RegimenCompleted_Total18to65,
            NotFullyVaccinated_18to65Prop = NotFullyVaccinated_18to65 / Population_18to65,

            # Calculate Vaccines Initiated 5-11
            RegimenInitiated_Total5to11 = RegimenInitiated_TotalGTE5 - RegimenInitiated_TotalGTE12,
            RegimenInitiated_Total5to11Prop = (RegimenInitiated_Total5to11)/Population_5to11,
            Initiated_NotCompleted_Total5to11 = RegimenInitiated_Total5to11 - RegimenCompleted_Total5to11,
            Initiated_NotCompleted_Total5to11Prop = Initiated_NotCompleted_Total5to11 / Population_5to11,
            Neither_Initiated_Nor_Completed_Total5to11 = Population_5to11 - RegimenInitiated_Total5to11,
            Neither_Initiated_Nor_Completed_Total5to11Prop = Neither_Initiated_Nor_Completed_Total5to11 / Population_5to11,

            # Calculate Vaccines Initiated 12-17
            RegimenInitiated_Total12to17 = RegimenInitiated_TotalGTE12 - RegimenInitiated_TotalGTE18,
            RegimenInitiated_Total12to17Prop = (RegimenInitiated_Total12to17)/Population_12to17,
            Initiated_NotCompleted_Total12to17 = RegimenInitiated_Total12to17 - RegimenCompleted_Total12to17,
            Initiated_NotCompleted_Total12to17Prop = Initiated_NotCompleted_Total12to17 / Population_12to17,
            Neither_Initiated_Nor_Completed_Total12to17 = Population_12to17 - RegimenInitiated_Total12to17,
            Neither_Initiated_Nor_Completed_Total12to17Prop = Neither_Initiated_Nor_Completed_Total12to17 / Population_12to17,

            # Calculate Vaccines Initiated 18 - 65
            RegimenInitiated_Total18to65 = RegimenInitiated_TotalGTE18 - RegimenInitiated_TotalGTE65,
            RegimenInitiated_Total18to65Prop = (RegimenInitiated_Total18to65)/Population_18to65,
            Initiated_NotCompleted_Total18to65 = RegimenInitiated_Total18to65 - RegimenCompleted_Total18to65,
            Initiated_NotCompleted_Total18to65Prop = Initiated_NotCompleted_Total18to65 / Population_18to65,

            #Calculate Pop Neither Initiated Nor Completed (ie. unvaccinated)
            Unvaccinated_Total18to65 = Population_18to65 - RegimenInitiated_Total18to65,
            Unvaccinated_Total18to65Prop = Unvaccinated_Total18to65 / Population_18to65,

            #Calculate 65+ Vaccination Columns
            Initiated_NotCompleted_TotalGTE65 = PopulationGTE65 - RegimenCompleted_TotalGTE65,
            Initiated_NotCompleted_TotalGTE65Prop = Initiated_NotCompleted_TotalGTE65 / PopulationGTE65,
            Unvaccinated_TotalGTE65 = PopulationGTE65 - RegimenInitiated_TotalGTE65,
            Unvaccinated_Total18to65Prop = Unvaccinated_TotalGTE65 / PopulationGTE65,
            NotFullyVaccinated_GTE65 = PopulationGTE65 - RegimenCompleted_TotalGTE65,
            NotFullyVaccinated_GTE65Prop = NotFullyVaccinated_GTE65 / PopulationGTE65,

            # Calculate Boosters 12-17
            Boosted_Total12to17 = Boosted_TotalGTE12 - Boosted_TotalGTE18,
            Boosted_Total12to17Prop = (Boosted_Total12to17)/Population_12to17,

            # Calculate Boosters 18 - 65
            Boosted_Total18to65 = Boosted_TotalGTE18 - Boosted_TotalGTE65,
            Boosted_Total18to65Prop = (Boosted_Total18to65)/Population_18to65,

            # Calculate Boosters GTE65
            Boosted_TotalGTE65Prop = (Boosted_TotalGTE65)/PopulationGTE65

        )


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Calculate Rolling Average Tables ####
    message(crayon::blue("Calculating 7 and 14 day rolling averages."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    varTable <- tibble::tribble(
        ~variable,                               ~Avg,         ~Total,     ~CalcString,
        "CasesNew",                              TRUE,         TRUE,       NA,
        "DeathsNew",                             TRUE,         TRUE,       NA,
        "TestsNew",                              TRUE,         TRUE,       NA,
        "CovidNew",                              TRUE,         TRUE,       NA,
        "CovidTotal",                            TRUE,         TRUE,       NA,
        "BedsUsedOther",                         TRUE,         TRUE,       "BedsUsed - CovidTotal",
        "BedsAvailable",                         TRUE,         TRUE,       NA,
        "CovidICUTotal",                         TRUE,         FALSE,      NA,
        "BedsICUUsedOther",                      TRUE,         FALSE,      "BedsICUUsed - CovidICUTotal",
        "BedsICUAvailable",                      TRUE,         FALSE,      NA,
        "CovidVentilatorsUsed",                  TRUE,         FALSE,      NA,
        "VentilatorsUsedOther",                  TRUE,         FALSE,      "VentilatorsUsed - CovidVentilatorsUsed",
        "VentilatorsAvailable",                  TRUE,         FALSE,      NA,
        "HospitalsReporting",                    TRUE,         TRUE,       NA,
        "HospitalsTotal",                        TRUE,         TRUE,       NA,
        "BedsInpatientUsedCovid",                TRUE,         FALSE,      NA,
        "BedsInpatientTotal",                    TRUE,         FALSE,      NA
    )


    cdtHosp7DayRollingData <- rollSummaryXDays(df = cdtHospData, numDays = 7, varTable = varTable)

    cdtHosp14DayRollingData <- rollSummaryXDays(df = cdtHospData, numDays = 14, varTable = varTable)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    out <- list(
        'cdtData' = cdtData, 'cdtNRData' = cdtNRData, 'hospData' = hospData, 'vaccData' = vaccData, "vaccCDCData" = vaccCDCData,
        'cdtHospData' = cdtHospData,
        'cdtHosp7DayRollingData' = cdtHosp7DayRollingData, 'cdtHosp14DayRollingData' = cdtHosp14DayRollingData
    )

    return(out)

}












