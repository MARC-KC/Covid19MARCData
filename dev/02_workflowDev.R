

#Variables
lagDays = 10
lagDaysHosp = 2









#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Load in CDT and Hospital Base Data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cdtData <- downloadBaseData(type = "CDT")

cdtNRData <-  downloadBaseData(type = "CDT_NewlyReported")

hospData <-  downloadBaseData(type = "Hospital")
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
        CovidTotal, #CovidConfirmed, CovidSuspected, ##############DELETE COMMENT ####
        CovidNew, #CovidNew24HConfirmed, CovidNew24HSuspected, ##############DELETE COMMENT ####
        CovidICUTotal, CovidICUConfirmed, CovidICUSuspected,
        VentilatorsTotal, VentilatorsUsed, VentilatorsAvailable,
        CovidVentilatorsUsed)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate Rolling Average Tables ####
cat(crayon::blue("Calculating 7 and 14 day rolling averages.\n"))
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


# Obtain Base Hospital Column Restriction Vector for Summaries (i.e., what columns deal with hospital data and may need restricted based on GeoID [only necessary for Internal MARC server data])
rolling7DayHospRestrictCols <- stringr::str_subset(names(cdtHosp7DayRollingData), "Jurisdiction|State|Region|GeoID|Date|CasesNew|DeathsNew|TestsNew", negate = TRUE)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



