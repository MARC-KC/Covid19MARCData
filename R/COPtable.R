
#' @title COPtable
#'
#' @description Create the dataset used by the COPtable with the multiple slicers
#'
#' @param df Input data from \code{rollSummaryXDays()}. Either \code{cdtHosp7DayRollingData} or \code{cdtHosp14DayRollingData}
#' @param days The number of days that the time period is over. Should match the days argument for the \code{numDays} argument of \code{rollSummaryXDays()}.
#' @param lagDays The number of days to lag the data for.
#' @param measureTable A formatted data.frame contianing the columns 'measureName', 'Avg_Total', 'measureDisplayName', 'upGood', 'PerCapita'
#' @param percentChangeKPI What percent change should the KPI's be activated for? Default is 5%.
#'
#' @export

COPtable <- function(df, days, lagDays, measureTable, percentChangeKPI = 5) {

    #Variables that can be changed to add more features
    measureTable <- measureTable %>%
        dplyr::mutate(fullMeasureName = glue::glue("{measureName}{days}Day{Avg_Total}"))


    #Join Base df and popTable and Organize Main Columns
    df <- df %>% dplyr::left_join(popTable, by = "GeoID") %>%
        dplyr::filter(Date <= (max(Date) - lagDays)) %>%
        dplyr::select(c("Jurisdiction", "State", "Region", "GeoID", "Date", glue::glue_data(measureTable, "{measureTable$measureName}{days}Day{Avg_Total}"), "Population"))

    #Create Per Capitia Columns, filter by the three dates needed for further calculations, and rename original columns
    calcTable <- measureTable %>% dplyr::filter(PerCapita) %>%
        dplyr::mutate(perCapitaCalcName = glue::glue("{measureDisplayName}Per100K"),
                      perCapitaCalc = glue::glue("{fullMeasureName} / Population * 100000"))

    #Select the last 3 time periods for each GeoID given the days argument
    df <- df %>%
        marcR::groupby_rank(GeoID, rankby = Date) %>%
        marcR::mutateCalcString(calcTable$perCapitaCalcName, calcTable$perCapitaCalc) %>%
        dplyr::filter(rankID %in% c(1, (days + 1), (days*2 + 1))) %>%
        dplyr::rename_with(.cols = all_of(measureTable$fullMeasureName), .fn = ~measureTable$measureName[measureTable$fullMeasureName == .x])

    #Lag the Date
    df <- df %>%
        dplyr::group_by(GeoID) %>%
        dplyr::mutate(PreviousDate = dplyr::lag(Date, n = 1, order_by = Date)) %>%
        dplyr::ungroup()

    #Lag the base and per capita fields and just filter the last 2 time periods
    calcTable <- tibble::tibble(baseName = names(df)[!(names(df) %in% c('Jurisdiction', 'State', 'Region', 'GeoID', 'Date', 'PreviousDate', 'Population', 'rankID'))],
                                newName = glue::glue("Previous{baseName}"),
                                calcString = glue::glue("dplyr::lag({baseName}, n=1, order_by=Date)"))
    df <- df %>% dplyr::group_by(GeoID) %>% dplyr::group_split() %>%
        purrr::map_dfr(~marcR::mutateCalcString(.x, calcTable$newName, calcTable$calcString)) %>%
        dplyr::filter(rankID %in% c(1, (days + 1)))


    #Calculate change in the base and per capita fields
    calcTable <- tibble::tibble(baseName = names(df)[!(names(df) %in% c('Jurisdiction', 'State', 'Region', 'GeoID', 'Date', 'PreviousDate', 'Population', 'rankID'))] %>% stringr::str_subset("^Previous", negate = TRUE),
                                prevName = glue::glue("Previous{baseName}"),
                                newName = glue::glue("Change{baseName}"),
                                calcString = glue::glue("{baseName} - {prevName}"))
    df <- df %>% dplyr::group_by(GeoID) %>% dplyr::group_split() %>%
        purrr::map_dfr(~marcR::mutateCalcString(.x, calcTable$newName, calcTable$calcString))

    #Calculate ratio of change and select teh last time period
    calcTable <- tibble::tibble(baseName = measureTable$measureName,
                                prevName = glue::glue("Previous{baseName}"),
                                newName = glue::glue("ChangeProp{baseName}"),
                                calcString = glue::glue("({baseName} / dplyr::if_else({prevName} == 0, dplyr::if_else({baseName} == 0, 1, NA_real_), as.double({prevName}))) - 1"))
    df <- df %>% dplyr::group_by(GeoID) %>% dplyr::group_split() %>%
        purrr::map_dfr(~marcR::mutateCalcString(.x, calcTable$newName, calcTable$calcString)) %>%
        dplyr::filter(rankID == 1) %>% dplyr::select(-rankID)


    #Pivot Table into Long Format
    measureTablePer100K <- measureTable %>% dplyr::filter(PerCapita)

    spec <- list(tibble::tibble(.name=measureTable$measureName, .value = "CurrentNew", Measure = measureTable$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "^Previous"), "Per100K|Date", negate = TRUE), .value = "PreviousNew", Measure = measureTable$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "^Change"), "Per100K|Prop", negate = TRUE), .value = "Change", Measure = measureTable$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "^Change"), "Prop"), .value = "ChangeProp", Measure = measureTable$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "Per100K$"), "^(Previous|Change)", negate = TRUE), .value = "CurrentNewPer100K", Measure = measureTablePer100K$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "Per100K$"), "^Previous"), .value = "PreviousNewPer100K", Measure = measureTablePer100K$measureDisplayName),
                 tibble::tibble(.name=stringr::str_subset(stringr::str_subset(names(df), "Per100K$"), "^Change"), .value = "ChangePer100K", Measure = measureTablePer100K$measureDisplayName)
    ) %>%
        dplyr::bind_rows()

    df <- df %>% tidyr::pivot_longer_spec(spec)


    #Add Columns Needed for slicers, week change ratio, and rename Date to CurrentDate
    df <- df %>% dplyr::mutate(DayPeriod = days, LagDays = lagDays) %>%
        dplyr::mutate(ChangeRatio = ChangeProp + 1) %>%
        dplyr::rename("CurrentDate" = "Date")


    #Add KPI indicator and color
    colorTable <- tibble::tribble(
        ~color,        ~hex,
        "green",       "#06C72F",
        "red",         "#ff3a22",#FF0008",
        "yellow",      "#E1E500"
    )

    propChange <- percentChangeKPI/100
    lowerPropChange <- 1 - propChange
    upperPropChange <- 1 + propChange



    df <- df %>% dplyr::mutate(
        KPI_ID = dplyr::case_when(
            (ChangeRatio <= lowerPropChange & Measure %in% measureTable$measureDisplayName[!measureTable$upGood]) | (is.na(ChangeRatio) & Change < 0 & Measure %in% measureTable$measureDisplayName[!measureTable$upGood]) ~ 1,
            (ChangeRatio <= lowerPropChange & Measure %in% measureTable$measureDisplayName[measureTable$upGood]) | (is.na(ChangeRatio) & Change < 0 & Measure %in% measureTable$measureDisplayName[measureTable$upGood]) ~ 2,
            ChangeRatio > lowerPropChange & ChangeRatio < upperPropChange ~ 3,
            (ChangeRatio >= upperPropChange & Measure %in% measureTable$measureDisplayName[measureTable$upGood]) | (is.na(ChangeRatio) & Change > 0 & Measure %in% measureTable$measureDisplayName[measureTable$upGood]) ~ 4,
            (ChangeRatio >= upperPropChange & Measure %in% measureTable$measureDisplayName[!measureTable$upGood]) | (is.na(ChangeRatio) & Change > 0 & Measure %in% measureTable$measureDisplayName[!measureTable$upGood]) ~ 5,
            TRUE ~ NA_real_
        ),
        KPI_Color = dplyr::case_when(
            KPI_ID %in% c(1,4) ~ colorTable[colorTable$color == 'green',]$hex,
            KPI_ID %in% c(2,5) ~ colorTable[colorTable$color == 'red',]$hex,
            KPI_ID == 3 ~ colorTable[colorTable$color == 'yellow',]$hex,
            TRUE ~ NA_character_
        )
    )


    return(df)

}




