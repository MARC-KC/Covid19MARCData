

#' @title Create School Gating Criteria Table
#'
#' @description Given the 14 day rolling averages, format the columns needed to
#'   run the school gating criteria page.
#'
#' @param df_14DayRolling The 14 day output from `rollSummaryXDays()`.
#'   'cdtHosp14DayRollingData' from `getBaseCovidData()`
#' @param lagDays The number of days to lag by. Defaults at 7 days which has
#'   been the historic number used for the school gating criteria page.
#'
#' @export
SGC_14DaySummary <- function(df_14DayRolling, lagDays = 7) {

    #Add basic columns for the School gating criteria
    df <- df_14DayRolling %>%
        dplyr::mutate(
            TestsPositivity14DayProportion = CasesNew14DayTotal/dplyr::if_else(TestsNew14DayTotal == 0, NA_integer_, TestsNew14DayTotal),
            DeathsToCases14DayProportion = DeathsNew14DayTotal/dplyr::if_else(CasesNew14DayTotal == 0, NA_integer_, CasesNew14DayTotal),
            HospsToCases14DayProportion = CovidNew14DayTotal/dplyr::if_else(CasesNew14DayTotal == 0, NA_integer_, CasesNew14DayTotal)
        )

    #Add Population (Per-Capita) Calculations
    df <- df %>%
        dplyr::left_join(y = dplyr::select(Covid19MARCData::popTable, GeoID, Population), by = "GeoID") %>%
        dplyr::mutate(
            CasesNew14DayTotalPer100K = CasesNew14DayTotal / Population * 100000
        )


    #Add Hospital Bed Caclulations
    df <- df %>%
        dplyr::mutate(
            #Calclulate Weighted Averages
            BedsAvailable14WAvg = as.integer(round(HospitalsTotal14DayTotal * BedsAvailable14DayTotal / HospitalsReporting14DayTotal / 14, 0)),
            BedsUsedCovid14WAvg = as.integer(round(HospitalsTotal14DayTotal * CovidTotal14DayTotal / HospitalsReporting14DayTotal / 14, 0)),
            BedsUsedOther14WAvg = as.integer(round(HospitalsTotal14DayTotal * BedsUsedOther14DayTotal / HospitalsReporting14DayTotal / 14, 0)),

            #Explicitly Calculate Totals using used and available
            BedsTotal14WAvg = (BedsAvailable14WAvg + BedsUsedOther14WAvg + BedsUsedCovid14WAvg),

            #Calculate Proportions of Use
            BedsAvailable14WAvg_Prop = BedsAvailable14WAvg/dplyr::if_else(BedsTotal14WAvg == 0, NA_integer_, BedsTotal14WAvg),
            BedsUsedCovid14WAvg_Prop = BedsUsedCovid14WAvg/dplyr::if_else(BedsTotal14WAvg == 0, NA_integer_, BedsTotal14WAvg),
            BedsUsedOther14WAvg_Prop = BedsUsedOther14WAvg/dplyr::if_else(BedsTotal14WAvg == 0, NA_integer_, BedsTotal14WAvg)
        )

    #Filter Data
    latestMonday <- Sys.Date() - as.numeric(format(Sys.Date(), format = "%u")) + 1
    df <- df %>%
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>%
        dplyr::filter(dayWeek == 1 & Date <= (latestMonday - lagDays)) %>%
        dplyr::select(
            Jurisdiction, State, Region, GeoID, Date,
            CasesNew14DayTotal,
            CasesNew14DayTotalPer100K,
            TestsPositivity14DayProportion,
            BedsAvailable14WAvg_Prop,
            BedsUsedCovid14WAvg_Prop,
            BedsUsedOther14WAvg_Prop
        )


    return(df)
}

