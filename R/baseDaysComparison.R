#' @title X Days Comparison
#'
#' @description Uses the output from `rollSummaryXDays()` and the formatted
#'   measure table to create comparisons. This is generalized so that it can be
#'   used for the 7 Day Comparison section and for the Weekly Data Snapshot
#'   section.
#'
#' @param rollSumData An output from `rollSummaryXDays()`.
#' @param measureTable A formatted table containing the columns measureName and
#'   upGood. measureName is the name of the columns you are wanting to create
#'   the comparison measure for with the number of days replaced by ##. upGood
#'   is a logical for each measureName on whether an increasing value is good or
#'   bad. This is used the create the KPI values.
#' @param days The number of days the rollSumData was calculated over.
#' @param lag The multiplier for the number of days to lag over. The default is
#'   the value set to days. This is added in for generalization for using
#'   pre-thinned data like in the Weekly Data Snapshot summaries.
#'
#' @export
baseDaysComparison <- function(rollSumData, measureTable, days = 7, lag = days) {

    #Fill in measureTable[['measureName']] with the argument days (allows the function to be more generalized)
    measureTable <- measureTable %>% dplyr::mutate(measureName = stringr::str_replace(measureName, "##", as.character(days)))

    #Pivot longer the measures and select out the columns to keep
    out <- rollSumData %>%
        tidyr::pivot_longer(cols = measureTable[['measureName']],
                            names_to = "Measure",
                            values_to = "CurrentWeekValue") %>%
        dplyr::select(Jurisdiction, State, Region, GeoID, Date, Measure, CurrentWeekValue) %>%
        dplyr::mutate(DatePrev = Date - days)

    #Group by GeoID and lag to caclulate changes for comparison
    out <- out %>%
        dplyr::group_by(GeoID) %>%
        dplyr::mutate(PreviousWeekValue = dplyr::lag(CurrentWeekValue, n = length(unique(Measure))*lag, order_by = Date)) %>%
        dplyr::mutate(WeekChange = CurrentWeekValue - PreviousWeekValue,
                      WeekChangeRatio = CurrentWeekValue/dplyr::if_else(PreviousWeekValue == 0, NA_real_, PreviousWeekValue)) %>%
        dplyr::mutate(WeekChangeRatio = dplyr::if_else(WeekChange == 0 & PreviousWeekValue == 0, 1, WeekChangeRatio)) %>%
        dplyr::mutate(WeekChangeProp = WeekChangeRatio - 1) %>%
        dplyr::mutate(
            KPI_ID = dplyr::case_when(
                (WeekChangeRatio <= 0.95 & Measure %in% measureTable$measureName[!measureTable$upGood]) | (is.na(WeekChangeRatio) & WeekChange < 0 & Measure %in% measureTable$measureName[!measureTable$upGood]) ~ 1,
                (WeekChangeRatio <= 0.95 & Measure %in% measureTable$measureName[measureTable$upGood]) | (is.na(WeekChangeRatio) & WeekChange < 0 & Measure %in% measureTable$measureName[measureTable$upGood]) ~ 2,
                WeekChangeRatio > 0.95 & WeekChangeRatio < 1.05 ~ 3,
                (WeekChangeRatio >= 1.05 & Measure %in% measureTable$measureName[measureTable$upGood]) | (is.na(WeekChangeRatio) & WeekChange > 0 & Measure %in% measureTable$measureName[measureTable$upGood]) ~ 4,
                (WeekChangeRatio >= 1.05 & Measure %in% measureTable$measureName[!measureTable$upGood]) | (is.na(WeekChangeRatio) & WeekChange > 0 & Measure %in% measureTable$measureName[!measureTable$upGood]) ~ 5,
                TRUE ~ NA_real_
            )) %>%
        dplyr::ungroup()

    return(out)
}

