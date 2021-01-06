#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - rollingXdayCalcs - Heavy Lifting for rolling averages ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @title Rolling Averages and Totals for A X Day Period
#'
#' @description Calculates the rolling averages and totals over the last X days. This is currently a helper function for `rollAvgXDays()`.
#'
#' @param df Input data.frame to be summarized
#' @param dateCol Character name for the date column
#' @param calcCol Character vector of the the column names used to summarize over.
#' @param Xdays The number of days to do the summary over
#' @param total TRUE/FALSE on whether X day totals should be caclulated. Either length 1 or length(calcCol) to pair it with a calcCol variable.
#' @param average TRUE/FALSE on whether X day averages should be caclulated. Either length 1 or length(calcCol) to pair it with a calcCol variable.
#'
#' @noRd
rollingXdayCalcs <- function(df, dateCol, calcCol, Xdays = 7, total = TRUE, average = TRUE) {

    if (length(calcCol) > 1) {
        if (length(total) == 1) {
            total <- rep(total, length(calcCol))
        } else if (length(total) != length(calcCol)) {
            stop("total must be length 1 or the same length as calcCol")
        }
        if (length(average) == 1) {
            average <- rep(total, length(calcCol))
        } else if (length(average) != length(calcCol)) {
            stop("average must be length 1 or the same length as calcCol")
        }
    }


    df <- as.data.frame(df)
    df_sep <- tibble::tibble(dfXDay = purrr::map(1:nrow(df), ~dplyr::filter(df, !!rlang::sym(dateCol) <= df[.x,dateCol] & !!rlang::sym(dateCol) >= df[.x,dateCol] - (Xdays - 1) & GeoID == df[.x,"GeoID"])))

    out <- purrr::map_dfc(1:length(calcCol), function(measureID) {



        out <- df_sep %>% dplyr::mutate(
            measureData = purrr::map(dfXDay, ~na.omit(.x[[calcCol[measureID]]])),
            nRows = purrr::map_int(measureData, ~length(na.omit(.x))),
            total = purrr::map2_int(nRows, measureData, ~dplyr::if_else(.x == 0, NA_integer_, sum(.y))),
            avg = purrr::map2_dbl(nRows, total, ~dplyr::if_else(.x == 0, NA_real_, (.y/.x)))
        )


        if (total[measureID] & average[measureID]) {
            out <- dplyr::select(out, total, avg)
            names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Total', 'Avg')}")
        } else if (total[measureID] & !average[measureID]) {
            out <- dplyr::select(out, total)
            names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Total')}")
        } else if (!total[measureID] & average[measureID]) {
            out <- dplyr::select(out, avg)
            names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Avg')}")
        }



        return(out)
    })


    return(out)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - rollAvgXDays - R Equivalent to the CDT and Hospital Rolling Summary Views ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#' @title Create Rolling Summaries Given a Number of Days
#'
#' @description Given a formatted variable table, this function calculates the
#'   rolling averages and totals for the specified columns over a specified
#'   number of days.
#'
#' @param df Input data.frame to be summarized
#' @param numDays The number of days to do the summary over. Defaults at 7 days.
#' @param varTable A formatted data.frame containing variable, Avg, Total, CalcString columns. See details for more information.
#'
#' @details The format of varTable:
#' \describe{
#'   \item{variable}{Variable names that you want to create summaries for}
#'   \item{Avg}{TRUE/FALSE on whether X day averages should be caclulated for the given variable}
#'   \item{Total}{TRUE/FALSE on whether X day totals should be caclulated for the given variable}
#'   \item{CalcString}{Used for pre-summary mutations to create the variables needed; see \code{\link[marcR]{mutateCalcString}}. If none are needed, supply a vector of NA's}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Load in CDT and Hospital Base Data
#' cdtData <- downloadBaseData(type = "CDT")
#' hospData <-  downloadBaseData(type = "Hospital")
#'
#'
#' # Combine Hospital and CDT Base Data
#' cdtHospData <- dplyr::full_join(cdtData, hospData, by = c("GeoID", "Date")) %>%
#'     marcR::coalesceJoin() %>%
#'     dplyr::mutate(CovidNew = CovidNew24HConfirmed + CovidNew24HSuspected) %>%
#'     dplyr::select(
#'         Jurisdiction, State, GeoID, Region, Date,
#'
#'         CasesNew, CasesTotal,
#'         DeathsNew, DeathsTotal,
#'         TestsNew, TestsTotal,
#'         Population,
#'
#'         HospitalsReporting, HospitalsTotal,
#'         BedsTotal, BedsUsed, BedsAvailable,
#'         BedsICUTotal, BedsICUUsed, BedsICUAvailable,
#'         CovidTotal,
#'         CovidNew,
#'         CovidICUTotal, CovidICUConfirmed, CovidICUSuspected,
#'         VentilatorsTotal, VentilatorsUsed, VentilatorsAvailable,
#'         CovidVentilatorsUsed)
#'
#' # Calculate Rolling Average Tables
#' varTable <- tibble::tribble(
#'     ~variable,                  ~Avg,         ~Total,     ~CalcString,
#'     "CasesNew",                 TRUE,         TRUE,       NA,
#'     "DeathsNew",                TRUE,         TRUE,       NA,
#'     "TestsNew",                 TRUE,         TRUE,       NA,
#'     "CovidNew",                 TRUE,         TRUE,       NA,
#'     "CovidTotal",               TRUE,         FALSE,      NA,
#'     "BedsUsedOther",            TRUE,         FALSE,      "BedsUsed - CovidTotal",
#'     "BedsAvailable",            TRUE,         FALSE,      NA,
#'     "CovidICUTotal",            TRUE,         FALSE,      NA,
#'     "BedsICUUsedOther",         TRUE,         FALSE,      "BedsICUUsed - CovidICUTotal",
#'     "BedsICUAvailable",         TRUE,         FALSE,      NA,
#'     "CovidVentilatorsUsed",     TRUE,         FALSE,      NA,
#'     "VentilatorsUsedOther",     TRUE,         FALSE,      "VentilatorsUsed - CovidVentilatorsUsed",
#'     "VentilatorsAvailable",     TRUE,         FALSE,      NA,
#'     "HospitalsReporting",       TRUE,         TRUE,       NA,
#'     "HospitalsTotal",           TRUE,         TRUE,       NA
#' )
#'
#'
#' cdtHosp7DayRollingData <- rollSummaryXDays(df = cdtHospData, numDays = 7, varTable = varTable)
#'
#' }
#'
#'
#' @export
rollSummaryXDays <- function(df, numDays = 7, varTable) {

    #Create Missing Calculated Variables
    calcTab <- varTable %>% dplyr::select(variable, CalcString) %>% dplyr::filter(!is.na(CalcString))

    df <- df %>% marcR::mutateCalcString(calcTab$variable, calcTab$CalcString)


    #Create previous date column (startDate)
    df <- df %>% dplyr::mutate(prevDate = Date - (numDays-1))


    DayDF <- rollingXdayCalcs(df = df, dateCol="Date", calcCol = varTable$variable, Xdays = numDays, total = varTable$Total, average = varTable$Avg)


    df <- df %>% dplyr::select(Jurisdiction, State, Region, GeoID, Date) %>% dplyr::bind_cols(DayDF)

    return(df)

}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
