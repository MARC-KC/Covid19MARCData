

#Variables
baseDataList <- downloadAllCovidAPIData()
lagDays = 10
lagDaysHosp = 2







#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load in the base data to the environment from a list ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
list2env(baseDataList, env = rlang::current_env())
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine Hospital and CDT Base Data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cdtHospData <- dplyr::full_join(cdtData, hospData, by = c("GeoID", "Date")) %>%
    marcR::coalesceJoin() %>%
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
message(crayon::blue("Calculating 7 and 14 day rolling averages.\n"))
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




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add Base Tables ####
message(crayon::blue("Exporting base CDT data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_CDT_TimeSeries <- cdtData

bi_CDT_NewlyReported <- cdtNRData %>% marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)

# bi_CDT_MostRecent <- bi_CDT_TimeSeries %>% marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full 7 Day  Rolling Summary With and Without Lag ####
message(crayon::blue("Exporting 7 day rolling averages and totals.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_7DayRolling <-
    dplyr::left_join(dplyr::mutate(cdtHosp7DayRollingData,
                                   TestsPositiveNew7DayAvgProportion = CasesNew7DayTotal/dplyr::if_else(TestsNew7DayTotal == 0, NA_integer_, TestsNew7DayTotal),
                                   DeathsToCases7DayProportion = DeathsNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal),
                                   HospsToCases7DayProportion = CovidNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal)),
                     dplyr::mutate(popTable,
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
        ))


# bi_7DayRollingLag <- bi_7DayRolling %>% dplyr::filter(Date <= (max(Date) - lagDays))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Weekly Thinned 7 Day Rolling With and Without Lag ####
message(crayon::blue("Exporting thinned 7 day rolling averages and totals.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# bi_7DayRollingThin <- bi_7DayRolling %>%
#     dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
#     dplyr::filter(dayWeek == dayWeek[which.max(Date)])

bi_7DayRollingThinLag <- bi_7DayRolling %>%
    dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                  CasesNew7DayTotal, CasesNew7DayAvg,
                  DeathsNew7DayTotal, DeathsNew7DayAvg,
                  TestsNew7DayTotal, TestsNew7DayAvg,
                  TestsPositiveNew7DayAvgProportion, DeathsToCases7DayProportion, HospsToCases7DayProportion,
                  Population, PopulationTestStandard, PositiveTestStandardProportion, PositiveTestStandard, PositiveTestStandard,
                  KPI_PositiveTests, KPI_PopulationTests) %>%
    dplyr::filter(Date <= (max(Date) - lagDays)) %>%
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
    dplyr::select(-dayWeek)

bi_7DayRollingThinLagHosp <- bi_7DayRolling %>%
    dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                  CovidNew7DayTotal, CovidNew7DayAvg,
                  HospitalsReporting7DayTotal, HospitalsReporting7DayAvg, HospitalsTotal7DayTotal, HospitalsTotal7DayAvg) %>%
    dplyr::filter(Date <= (max(Date) - lagDaysHosp)) %>%
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
    dplyr::select(-dayWeek)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7 Day Comparison - Last 6 Weeks and Most Recent With and Without Lag ####
message(crayon::blue("Exporting 7 day comparison sheets.\n"))
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

bi_7DayComparison_MostRecent_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - lagDays)) %>%
    marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1)

bi_7DayComparison_MostRecent_HospLag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - lagDaysHosp)) %>%
    marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1)


bi_7DayComparison_Last6Weeks <- baseWeeklyComparisonData %>% dplyr::filter(Date >= (max(Date, na.rm = TRUE) - lubridate::weeks(6)))

bi_7DayComparison_Last6Weeks_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date >= ((max(Date, na.rm = TRUE) - lagDays) - lubridate::weeks(6)) & (Date <= ((max(Date, na.rm = TRUE) - lagDays))))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full Hospital Data WIth Calculations And Most Recent ####
message(crayon::blue("Exporting base hospital data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_HospitalDailyData <- hospData %>%
    dplyr::mutate(CovidNew = CovidNew24HConfirmed + CovidNew24HSuspected) %>%
    dplyr::mutate(
        #Calculate Used by Other Columns
        BedsUsedOther = (BedsUsed - CovidTotal),
        BedsICUUsedOther = (BedsICUUsed - CovidICUTotal),
        VentilatorsUsedOther = (VentilatorsUsed - CovidVentilatorsUsed),

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
        CovidVentilatorsUsedProportion = CovidVentilatorsUsed/dplyr::if_else(VentilatorsTotal == 0, NA_integer_, VentilatorsTotal)
    )

#HospitalTotal based on a 3 week window so that it can adapt to reporting over time
bi_HospitalDailyData <- bi_HospitalDailyData %>%
    dplyr::mutate(
        HospitalsTotal = purrr::map2_int(GeoID, Date, ~dplyr::filter(bi_HospitalDailyData, GeoID == .x & Date >= .y - 10 & Date <= .y + 10)[['HospitalsReporting']] %>% max(., na.rm = TRUE) %>% as.integer())
    )

bi_HospitalMostRecent <- bi_HospitalDailyData %>%
    marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Testing Page tables ####
# Used to create the tables for the tesing page. Mainly the need for negative vs positive tests
message(crayon::blue("Exporting tables for testing page.\n"))
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
    dplyr::filter(Date <= max(Date) - lagDays)

bi_TestingPage7DayRollingThinLag <- ct7DayRollingData %>%
    dplyr::filter(Date <= max(Date) - lagDays) %>%
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>%
    dplyr::select(-dayWeek)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Jurisdiction Bar Charts given time scenarios ####
message(crayon::blue("Exporting jurisdiction bar chart data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mostRecentGivenHelperTable <- tibble::tribble(
    ~datasetName,         ~days, ~lagDays, ~keep,
    "cdtHospData",        7,     lagDays,  "Both",
    "cdtHospData",        14,    lagDays,  "Both",
    "cdtHospData",        30,    lagDays,  "Both",
    "cdtHospData",        60,    lagDays,  "Both",
    "cdtHospData",        90,    lagDays,  "Both",
    "cdtHospData",        NA,    NA,       "Both"
)

bi_JurisdictionBarCharts <- purrr::pmap_dfr(mostRecentGivenHelperTable, function(datasetName, days, lagDays, keep, ...) {
    dataset <- eval(rlang::sym(datasetName))

    out <- mostRecentGivenTime(df = dataset, days=days, lagDays=lagDays)

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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PrettyJurisdictions ####
# Used as a Bridge table in the Power BI relationships
message(crayon::blue("Exporting jurisdiction bridge table with the formatted names.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bi_PrettyJurisdictions_MARC <- prettyJurisdictions %>% dplyr::filter(Site == 'MARC') %>% dplyr::select(-Site)
bi_PrettyJurisdictions_HCC <- prettyJurisdictions %>% dplyr::filter(Site == 'HCC') %>% dplyr::select(-Site)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# HelperTable ####
# Used to help create measures in Power BI
message(crayon::blue("Exporting helper table for PowerBI measures.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_HelperTable <- tibble::tribble(
    ~HelperID,          ~DateTime,
    "LastExport",         Sys.time()
)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






out <- mget(stringr::str_subset(ls(), "^bi_"))




objectList = out
outputFolder = '//KCJazz/GIS/DataDevelopment/HumanServices/COVID-19/Outputs/PipelineDataOutputs/PublishData'
list2CSV(objectList, outputFolder)


