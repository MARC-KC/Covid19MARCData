

#' @title Create Weekly Data Snapshot Data for Power Bi
#' @description   Given the base datasets, this function will do all the
#'   transformations and summarizations that are used to feed the data being
#'   displayed on the Weekly Data Snapshot

#' @export
createBiDatasets_WDS <- function(baseDataList = getBaseCovidData(), cutoffDay = 'Sunday', lagDaysCDT = 10, lagDaysHosp = 2) {



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Load in the base data to the environment from a list ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    list2env(baseDataList, env = rlang::current_env())
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    message(crayon::blue("Exporting Weekly Data Snapshot Data."))



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Get the cutoff days of the week as integers ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    cutoffDayInt <- as.integer(
        factor(cutoffDay,
               levels = c("Monday", "Tuesday", "Wednesday",
                          "Thursday", "Friday", "Saturday", "Sunday"),
               ordered = TRUE)
    )

    cutoffDayInt_CDTlag <- ifelse(((cutoffDayInt - lagDaysCDT) %% 7) == 0, 7, ((cutoffDayInt - lagDaysCDT) %% 7))
    cutoffDayInt_hosplag <- ifelse(((cutoffDayInt - lagDaysHosp) %% 7) == 0, 7, ((cutoffDayInt - lagDaysHosp) %% 7))
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Get full time series data ####
    # Data is filtered so only one point per week based on the most recent Day of Week
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    #Create CDT time series with the cutoff date and lagDaysCDT
    bi_WDS_7DayRollingLag <- cdtHosp7DayRollingData %>% dplyr::filter(Date <= max(Date) - lagDaysCDT) %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == cutoffDayInt_CDTlag) %>% dplyr::select(-dayWeek) %>%
        dplyr::mutate(TestsPositive7DayRate = CasesNew7DayTotal/dplyr::if_else(TestsNew7DayTotal == 0, NA_integer_, TestsNew7DayTotal),
                      DeathsToCases7DayRate = DeathsNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal),
                      HospsToCases7DayRate = CovidNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal)) %>%
        dplyr::filter(GeoID %in% c('MARC', '20MARCReg', '29MARCReg', 'HCC')) %>%
        dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                      CasesNew7DayTotal, CasesNew7DayAvg, DeathsNew7DayTotal, DeathsNew7DayAvg, TestsNew7DayTotal, TestsNew7DayAvg,
                      TestsPositive7DayRate, DeathsToCases7DayRate, HospsToCases7DayRate)

    #Most recent version of the last dataset
    bi_WDS_Last7Days <- bi_WDS_7DayRollingLag %>%
        marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1) %>%
        dplyr::filter(GeoID %in% c('MARC', 'HCC'))


    #Create Hospital time series with the cutoff date and lagDaysHosp
    bi_WDS_7DayRollingLagHosp <- cdtHosp7DayRollingData %>% dplyr::filter(Date <= max(Date) - lagDaysHosp) %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == cutoffDayInt_hosplag) %>% dplyr::select(-dayWeek) %>%
        dplyr::filter(GeoID %in% c('MARC', '20MARCReg', '29MARCReg', 'HCC')) %>%
        dplyr::mutate(HospitalsTotal7DayTotal = dplyr::if_else(GeoID == 'MARC', as.integer(27*7), as.integer(HospitalsTotal7DayTotal))) %>%
        dplyr::mutate(Hospital7DayReportRate = HospitalsReporting7DayTotal/dplyr::if_else(HospitalsTotal7DayTotal == 0, NA_integer_, HospitalsTotal7DayTotal)) %>%
        dplyr::select(Jurisdiction, State, Region, GeoID, Date,
                      CovidNew7DayTotal, CovidNew7DayAvg, CovidTotal7DayAvg, Hospital7DayReportRate)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # #Weekly comparison for the last 6 weeks (last 6*7 days) ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    measureTableCDT <- tibble::tribble(
        ~measureName,                    ~upGood,
        "CasesNew7DayAvg",               FALSE,
        "CasesNew7DayTotal",             FALSE,
        "DeathsNew7DayAvg",              FALSE,
        "DeathsNew7DayTotal",            FALSE,
        "TestsNew7DayAvg",               TRUE,
        "TestsNew7DayTotal",             TRUE,
        "TestsPositive7DayRate",         FALSE,
        "DeathsToCases7DayRate",         FALSE,
        "HospsToCases7DayRate",          FALSE
    )
    measureTableHosp <- tibble::tribble(
        ~measureName,                    ~upGood,
        "CovidTotal7DayAvg",             FALSE,
        "CovidNew7DayAvg",               FALSE,
        "CovidNew7DayTotal",             FALSE,
        "Hospital7DayReportRate",        TRUE
    )

    bi_WDS_WeeklyComparison <-  dplyr::bind_rows(dplyr::filter(baseDaysComparison(bi_WDS_7DayRollingLag, measureTableCDT, days = 7, lag = 1), Date >= (max(Date) - 6*7)),
                                                 dplyr::filter(baseDaysComparison(bi_WDS_7DayRollingLagHosp, measureTableHosp, days = 7, lag = 1), Date >= (max(Date) - 6*7))
    ) %>% dplyr::filter(GeoID %in% c('MARC', 'HCC'))

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # #Weekly comparison table for all measures for easy display with slicers in the power bi helper ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
        "CovidNew##DayTotal",             FALSE
    )


    WDScomparison <- function(rollSumData, measureTable, days, lagDays) {
        rollSumData %>%
            dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
            dplyr::filter(Date <= max(Date[dayWeek == cutoffDayInt])) %>%
            dplyr::select(-dayWeek) %>%
            baseDaysComparison(., measureTable, days = days) %>%
            dplyr::filter(Date <= (max(Date) - lagDays)) %>%
            marcR::groupby_rank(GeoID, Measure, rankby = Date, filterIDs = 1) %>%
            # dplyr::group_by(GeoID, Measure) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>%
            dplyr::filter(GeoID == 'MARC') %>% #dplyr::ungroup() %>%
            dplyr::mutate(Days = days) %>% dplyr::rename_with(~stringr::str_remove(.x, "Week"), tidyr::contains("Week")) %>%
            dplyr::mutate(Lag = lagDays)
    }

    bi_WDS_ComparisonTable <- list(
        WDScomparison(cdtHosp7DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = TRUE),], days = 7, lagDays = lagDaysCDT),
        WDScomparison(cdtHosp7DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = FALSE),], days = 7, lagDays = lagDaysHosp),
        WDScomparison(cdtHosp14DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = TRUE),], days = 14, lagDays = lagDaysCDT),
        WDScomparison(cdtHosp14DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = FALSE),], days = 14, lagDays = lagDaysHosp)
    ) %>% dplyr::bind_rows() %>%
        dplyr::mutate(ChangeProp = ChangeRatio - 1)

    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # #WDS Time Helper table ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    minDate_CDT <- min(dplyr::filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = TRUE))[["Date"]])
    minDate_hosp <- min(dplyr::filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = FALSE))[["Date"]])

    bi_WDS_HelperTable <- tibble::tribble(
        ~ID_Name,              ~Date1,                    ~Date2,
        "CurrentDate",         Sys.Date(),                NA,
        "ReportCutoffDate",    minDate_CDT + lagDaysCDT,  NA,
        "CDTRange",            minDate_CDT - 6,           minDate_CDT,
        "HospRange",           minDate_hosp - 6,          minDate_hosp

    )
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # #WDS Time series data ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    bi_WDS_HospitalDailyData <- hospData %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(Date <= max(Date[dayWeek==cutoffDayInt])-lagDaysHosp) %>%
        dplyr::select(-dayWeek)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # Create return output ####
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    out <- mget(stringr::str_subset(ls(), "^bi_"))


    return(out)
    #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

}
