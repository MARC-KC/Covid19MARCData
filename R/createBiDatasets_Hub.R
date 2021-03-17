
#' @title Create the COVID-19 Data Hub Dataset for Power Bi
#'
#' @description Given the base datasets, this function will do all the
#'   transformations and summarizations that are used to feed the data being
#'   displayed on the MARC's COVID Data Hub.
#'
#' @param baseDataList A named list of data.frames containing the base data. See
#'   details for more information. Defaults to the return from
#'   \code{getBaseCovidData()}
#' @param lagDaysCDT Number of days to lag the Case, Death, Test data. Defaults
#'   to the value used by the Hub (10).
#' @param lagDaysHosp Number of days to lag the Hospital data. Defaults to the
#'   value used by the Hub (2).
#' @details \code{baseDataList} should contain a named list of the base data.frames. These
#'   are available through the MARC data API through the helpful functions
#'   \code{downloadMARCCovidData()} and \code{downloadAllCovidAPIData()}, but
#'   also must include the base summary datasets calculated from these. In total,
#'   this should include the 3 base data.frames and the 3 summary data.frames
#'   with the following names:
#' \describe{
#'   \item{cdtData}{Case, Death, and Test Data}
#'   \item{cdtNRData}{Newly Reported Case, Death, and Test Data}
#'   \item{hospData}{Hospital Data: modified by \code{getBaseCovidData}}
#'   \item{cdtHospData}{A joined version of \code{cdtData} and \code{hospData}}
#'   \item{cdtHosp7DayRollingData}{The 7 day rolling average of summary of cdtHospData}
#'   \item{cdtHosp14DayRollingData}{The 14 day rolling average of summary of cdtHospData}
#' }
#'
#' @return A list of data.frames that are used by MARC's COVID Data Hub.
#'
#' @export

createBiDatasets_Hub <- function(baseDataList = getBaseCovidData(), lagDaysCDT = 10, lagDaysHosp = 2) {



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Load in the base data to the environment from a list ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    list2env(baseDataList, env = rlang::current_env())
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Add Base Tables ####
    message(crayon::blue("Exporting base CDT data."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_CDT_TimeSeries <- cdtData

    bi_CDT_NewlyReported <- cdtNRData %>% marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)

    # bi_CDT_MostRecent <- bi_CDT_TimeSeries %>% marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Full Hospital Data WIth Calculations And Most Recent ####
    message(crayon::blue("Exporting base hospital data."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_HospitalDailyData <- hospData

    bi_HospitalMostRecent <- hospData %>%
        marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Full 7 Day  Rolling Summary With and Without Lag ####
    message(crayon::blue("Exporting 7 day rolling averages and totals."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_7DayRolling <-
        dplyr::left_join(dplyr::mutate(cdtHosp7DayRollingData,
                                       TestsPositiveNew7DayAvgProportion = CasesNew7DayTotal/dplyr::if_else(TestsNew7DayTotal == 0, NA_integer_, TestsNew7DayTotal),
                                       DeathsToCases7DayProportion = DeathsNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal),
                                       HospsToCases7DayProportion = CovidNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal)),
                         dplyr::mutate(Covid19MARCData::popTable,
                                       PopulationTestStandard = (ceiling(Population / 100000) * 150),
                                       PositiveTestStandardProportion = 0.05,
                                       PositiveTestStandard = 5),
                         by = "GeoID") %>%
        dplyr::mutate(
            KPI_PositiveTests = dplyr::case_when(
                TestsPositiveNew7DayAvgProportion < PositiveTestStandardProportion ~ 1,
                TestsPositiveNew7DayAvgProportion > PositiveTestStandardProportion ~ 3,
                TRUE ~ 2
            ),
            KPI_PopulationTests = dplyr::case_when(
                TestsNew7DayAvg < PopulationTestStandard ~ 3,
                TestsNew7DayAvg > PopulationTestStandard ~ 1,
                TRUE ~ 2
            )
        ) %>%
        dplyr::select(
            Jurisdiction, State, Region, GeoID, Date,
            CasesNew7DayTotal, CasesNew7DayAvg,
            DeathsNew7DayTotal, DeathsNew7DayAvg,
            TestsNew7DayTotal, TestsNew7DayAvg,
            TestsPositiveNew7DayAvgProportion, DeathsToCases7DayProportion, HospsToCases7DayProportion,
            Population, PopulationTestStandard, KPI_PopulationTests,
            PositiveTestStandardProportion, PositiveTestStandard, KPI_PositiveTests,
            CovidNew7DayTotal, CovidNew7DayAvg,
            HospitalsReporting7DayTotal, HospitalsReporting7DayAvg,
            HospitalsTotal7DayTotal, HospitalsTotal7DayAvg
        )


    # bi_7DayRollingLag <- bi_7DayRolling %>% dplyr::filter(Date <= (max(Date) - lagDaysCDT))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Weekly 7 Day Rolling With and Without Lag ####
    message(crayon::blue("Exporting 7 day rolling averages and totals with Lags."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_7DayRollingLag <- bi_7DayRolling %>%
        dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                      CasesNew7DayTotal, CasesNew7DayAvg,
                      DeathsNew7DayTotal, DeathsNew7DayAvg,
                      TestsNew7DayTotal, TestsNew7DayAvg,
                      TestsPositiveNew7DayAvgProportion, DeathsToCases7DayProportion, HospsToCases7DayProportion,
                      Population, PopulationTestStandard, PositiveTestStandardProportion, PositiveTestStandard, PositiveTestStandard,
                      KPI_PositiveTests, KPI_PopulationTests) %>%
        dplyr::filter(Date <= (max(Date) - lagDaysCDT))

    bi_7DayRollingLagHosp <- bi_7DayRolling %>%
        dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                      CovidNew7DayTotal, CovidNew7DayAvg,
                      HospitalsReporting7DayTotal, HospitalsReporting7DayAvg, HospitalsTotal7DayTotal, HospitalsTotal7DayAvg) %>%
        dplyr::filter(Date <= (max(Date) - lagDaysHosp))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Weekly Thinned 7 Day Rolling With and Without Lag ####
    message(crayon::blue("Exporting thinned 7 day rolling averages and totals with Lags."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # bi_7DayRollingThin <- bi_7DayRolling %>%
    #     dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
    #     dplyr::filter(dayWeek == dayWeek[which.max(Date)])

    bi_7DayRollingThinLag <- bi_7DayRollingLag %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
        dplyr::select(-dayWeek)

    bi_7DayRollingThinLagHosp <- bi_7DayRollingLagHosp %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
        dplyr::select(-dayWeek)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # 7 Day Comparison - Last 6 Weeks and Most Recent With and Without Lag ####
    message(crayon::blue("Exporting 7 day comparison sheets."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    measureTable <- tibble::tribble(
        ~measureName,                    ~upGood,
        "CasesNew##DayAvg",               FALSE,
        "CasesNew##DayTotal",             FALSE,
        "DeathsNew##DayAvg",              FALSE,
        "DeathsNew##DayTotal",            FALSE,
        "TestsNew##DayAvg",               TRUE,
        "TestsNew##DayTotal",             TRUE,
        "CovidTotal##DayAvg",             FALSE,
        "CovidNew##DayAvg",               FALSE,
        "CovidNew##DayTotal",             FALSE,
        "CovidICUTotal##DayAvg",          FALSE,
        "CovidVentilatorsUsed##DayAvg",   FALSE
    )



    baseWeeklyComparisonData <- baseDaysComparison(cdtHosp7DayRollingData, measureTable)


    bi_7DayComparison_MostRecent <- baseWeeklyComparisonData %>%
        marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1)

    bi_7DayComparison_MostRecent_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - lagDaysCDT)) %>%
        marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1)

    bi_7DayComparison_MostRecent_HospLag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - lagDaysHosp)) %>%
        marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1)


    bi_7DayComparison_Last6Weeks <- baseWeeklyComparisonData %>% dplyr::filter(Date >= (max(Date, na.rm = TRUE) - lubridate::weeks(6)))
    bi_7DayComparison_Last6Weeks_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date >= ((max(Date, na.rm = TRUE) - lagDaysCDT) - lubridate::weeks(6)) & (Date <= ((max(Date, na.rm = TRUE) - lagDaysCDT))))
    bi_7DayComparison_Last6Weeks_HospLag <- baseWeeklyComparisonData %>% dplyr::filter(Date >= ((max(Date, na.rm = TRUE) - lagDaysHosp) - lubridate::weeks(6)) & (Date <= ((max(Date, na.rm = TRUE) - lagDaysHosp))))


    bi_7DayComparison_AllTime <- baseWeeklyComparisonData
    bi_7DayComparison_AllTime_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date, na.rm = TRUE) - lagDaysCDT))
    bi_7DayComparison_AllTime_HospLag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date, na.rm = TRUE) - lagDaysHosp))


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Testing Page tables ####
    # Used to create the tables for the tesing page. Mainly the need for negative vs positive tests
    message(crayon::blue("Exporting tables for testing page."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    varTable <- tibble::tribble(
        ~variable,                  ~Avg,         ~Total,     ~CalcString,
        "TestsPositiveNew",         TRUE,         TRUE,       "CasesNew",
        "TestsNew",                 TRUE,         TRUE,       NA,
        "TestsNegativeNew",         TRUE,         TRUE,       NA
    )



    ct7DayRollingData <- cdtData %>%
        dplyr::mutate(TestsNegativeNew = TestsNew - CasesNew) %>%
        rollSummaryXDays(df = ., numDays = 7, varTable = varTable) %>%
        dplyr::mutate(TestsPositivity = dplyr::if_else(TestsNew7DayTotal == 0, NA_real_, TestsPositiveNew7DayTotal / TestsNew7DayTotal))


    bi_TestingPage7DayRollingLag <- ct7DayRollingData %>%
        dplyr::filter(Date <= max(Date) - lagDaysCDT)

    bi_TestingPage7DayRollingThinLag <- ct7DayRollingData %>%
        dplyr::filter(Date <= max(Date) - lagDaysCDT) %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
        dplyr::select(-dayWeek)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Jurisdiction Bar Charts given time scenarios ####
    message(crayon::blue("Exporting jurisdiction bar chart data."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    mostRecentGivenHelperTable <- tibble::tribble(
        ~datasetName,         ~days, ~lagDays, ~keep,
        "cdtHospData",        7,     lagDaysCDT,  "Both",
        "cdtHospData",        14,    lagDaysCDT,  "Both",
        "cdtHospData",        30,    lagDaysCDT,  "Both",
        "cdtHospData",        60,    lagDaysCDT,  "Both",
        "cdtHospData",        90,    lagDaysCDT,  "Both",
        "cdtHospData",        NA,    NA,          "Both"
    )

    bi_JurisdictionBarCharts <- purrr::pmap_dfr(mostRecentGivenHelperTable, function(datasetName, days, lagDays, keep, ...) {
        dataset <- eval(rlang::sym(datasetName))

        out <- mostRecentGivenTime_CDT(df = dataset, days=days, lagDays=lagDays)

        if (keep == "Both") {
            return(out)
        } else if (keep == "Raw") {
            return(dplyr::filter(out, Raw_Per100K == "Raw"))
        } else if (keep == "Per100K") {
            return(dplyr::filter(out, Raw_Per100K == "Per100K"))
        } else {
            warning("The argument keep must be one of 'Both', 'Raw', or 'Per100K'. Returning NULL")
            return(NULL)
        }
    })
    bi_JurisdictionBarCharts <- bi_JurisdictionBarCharts %>%
        dplyr::mutate(Raw_Per100K = dplyr::case_when(
            Raw_Per100K == 'Per100K' ~ glue::glue('Total {Measure} Per 100K'),
            Raw_Per100K == 'Raw' ~ glue::glue('Total {Measure}')
        ))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # COP Table ####
    # Used to create the main dynamic table with data on the COP page
    message(crayon::blue("Exporting COP comparison table with the formatted names."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    measureTable <- tibble::tribble(
        ~measureName,         ~Avg_Total,  ~measureDisplayName,   ~upGood,  ~PerCapita,
        "CasesNew",           "Total",     "Cases",               FALSE,    TRUE,
        "DeathsNew",          "Total",     "Deaths",              FALSE,    TRUE,
        "TestsNew",           "Total",     "Tests",               TRUE,     TRUE
    )

    bi_COPTable <- list(
        COPtable(cdtHosp7DayRollingData, days = 7, lagDays = lagDaysCDT, measureTable = measureTable, percentChangeKPI = 5),
        COPtable(cdtHosp14DayRollingData, days = 14, lagDays = lagDaysCDT, measureTable = measureTable, percentChangeKPI = 5)#,
        # COPtable(cdtHosp7DayRollingData, days = 7, lagDays = 0, measureTable = measureTable, percentChangeKPI = 5),
        # COPtable(cdtHosp14DayRollingData, days = 14, lagDays = 0, measureTable = measureTable, percentChangeKPI = 5)
    ) %>% dplyr::bind_rows()
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Vaccine Tables ####
    message(crayon::blue("Exporting Vaccine Tables"))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ## Create Base Calculated Columns ####
    bi_vacc_DailyData <- vaccData %>%
        dplyr::group_by(GeoID) %>%
        dplyr::mutate(
            DosesAdministered_New = DosesAdministered_Total - dplyr::lag(DosesAdministered_Total, n = 1, order_by = Date),
            RegimenInitiated_New = RegimenInitiated_Count - dplyr::lag(RegimenInitiated_Count, n = 1, order_by = Date),
            RegimenCompleted_New = RegimenCompleted_Count - dplyr::lag(RegimenCompleted_Count, n = 1, order_by = Date)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(GeoID) %>%
        dplyr::rename("RegimenInitiated_Total" = "RegimenInitiated_Count",
                      "RegimenCompleted_Total" = "RegimenCompleted_Count") %>%
        dplyr::mutate(
            RegimenInitiated_PropPop = RegimenInitiated_Total / Population,
            RegimenCompleted_PropPop = RegimenCompleted_Total / Population
        )



    ## Create 7-Day Rolling Averages and Totals ####
    varTable <- tibble::tribble(
        ~variable,                  ~Avg,         ~Total,     ~CalcString,
        "DosesAdministered_New",    TRUE,         TRUE,       NA,
        "RegimenInitiated_New",    TRUE,         TRUE,       NA,
        "RegimenCompleted_New",   TRUE,         TRUE,       NA
    )


    bi_vacc_7DayRollingData <- rollSummaryXDays(df = bi_vacc_DailyData, numDays = 7, varTable = varTable)



    ## Create 7-Day Comparison Table ####
    measureTable <- tibble::tribble(
        ~measureName,                            ~upGood,
        "DosesAdministered_New##DayTotal",       TRUE,
        "DosesAdministered_New##DayAvg",         TRUE,
        "RegimenInitiated_New##DayTotal",       TRUE,
        "RegimenInitiated_New##DayAvg",         TRUE,
        "RegimenCompleted_New##DayTotal",      TRUE,
        "RegimenCompleted_New##DayAvg",        TRUE
    )

    bi_vacc_baseWeeklyComparisonData <- baseDaysComparison(bi_vacc_7DayRollingData, measureTable)



    ## Create Jurisdiction Bar Chart Table ####
    mostRecentGivenHelperTable_Vacc <- tibble::tribble(
        ~datasetName,               ~days, ~lagDays,    ~keep,
        "bi_vacc_DailyData",        7,     0,           "Both",
        "bi_vacc_DailyData",        14,    0,           "Both",
        # "bi_vacc_DailyData",        30,    0,           "Both",
        # "bi_vacc_DailyData",        60,    0,           "Both",
        # "bi_vacc_DailyData",        90,    0,           "Both",
        "bi_vacc_DailyData",        NA,    NA,          "Both"
    )

    bi_vacc_JurisdictionBarCharts <- purrr::pmap_dfr(mostRecentGivenHelperTable_Vacc, function(datasetName, days, lagDays, keep, ...) {
        dataset <- eval(rlang::sym(datasetName))

        out <- mostRecentGivenTime_Vacc(df = dataset, days=days, lagDays=lagDays)

        if (keep == "Both") {
            return(out)
        } else if (keep == "Raw") {
            return(dplyr::filter(out, Raw_Per100K == "Raw"))
        } else if (keep == "Per100K") {
            return(dplyr::filter(out, Raw_Per100K == "Per100K"))
        } else {
            warning("The argument keep must be one of 'Both', 'Raw', or 'Per100K'. Returning NULL")
            return(NULL)
        }
    })
    bi_vacc_JurisdictionBarCharts <- bi_vacc_JurisdictionBarCharts %>%
        dplyr::filter(Measure == "DosesAdministered") %>%
        dplyr::mutate(
            RegimenInitiated_PropPop = dplyr::case_when(
                Raw_Per100K == 'Per100K' ~ RegimenInitiated / 100000,
                Raw_Per100K == 'Raw' ~ RegimenInitiated / Population
            ),
            RegimenCompleted_PropPop = dplyr::case_when(
                Raw_Per100K == 'Per100K' ~ RegimenCompleted / 100000,
                Raw_Per100K == 'Raw' ~ RegimenCompleted / Population
            )
        )%>%
        dplyr::mutate(Raw_Per100K = dplyr::case_when(
            Raw_Per100K == 'Per100K' ~ glue::glue('Total Administered Per 100K'),
            Raw_Per100K == 'Raw' ~ glue::glue('Total Administered')
        )) %>%
        dplyr::relocate(SlicerLevels, filterLevels, .after = dplyr::last_col())


    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # School Gating Criteria ####
    # Used to create the main table for the School Gating Criteria Page
    message(crayon::blue("Exporting School Gating Criteria Datasets."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_SGC_14DaySummary <- SGC_14DaySummary(df_14DayRolling = cdtHosp14DayRollingData, lagDays = 7)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # PrettyJurisdictions ####
    # Used as a Bridge table in the Power BI relationships
    message(crayon::blue("Exporting jurisdiction bridge table with the formatted names."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    bi_PrettyJurisdictions_MARC <- Covid19MARCData::prettyJurisdictions %>% dplyr::filter(Site == 'MARC') %>% dplyr::select(-Site) %>%
        dplyr::left_join(Covid19MARCData::popTable, by = "GeoID")
    bi_PrettyJurisdictions_HCC <- Covid19MARCData::prettyJurisdictions %>% dplyr::filter(Site == 'HCC') %>% dplyr::select(-Site) %>%
        dplyr::left_join(Covid19MARCData::popTable, by = "GeoID")
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # HelperTable ####
    # Used to help create measures in Power BI
    message(crayon::blue("Exporting helper table for PowerBI measures."))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_HelperTable <- tibble::tribble(
        ~HelperID,          ~DateTime,
        "LastExport",         Sys.time()
    )
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Create return output ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    out <- mget(stringr::str_subset(ls(), "^bi_"))
    names(out) <- stringr::str_remove(names(out), "^bi_")

    return(out)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



}



