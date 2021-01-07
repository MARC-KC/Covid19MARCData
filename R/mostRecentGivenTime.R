







#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FNs - mostRecentGivenTime - Creates data formatted bar charts with time slicers on CDT pages####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @title Most Recent Data Given A Time Period
#'
#' @description Used to create the jurisdiction bar charts with time and per
#'   capita/raw data slicers for the Case, Death, and Testing power bi pages
#'
#' @param df Input data.frame. It is currently absorbing 'cdtHospData'.
#' @param days Number of days to use for the time frame. A value of NA will trigger the 'All Time' summary.
#' @param lagdays Number of days to lag the time frame.
#'
#' @details Currently only set up to create summaries for Cases, Deaths, Tests in the time period.
#'
#' @export
mostRecentGivenTime <- function(df, days, lagDays) {


    #If days is NA, do the 'All Time' summary below
    if (!is.na(days)) {

        #Set up the calcTable helper.
        calcTable <- tibble::tribble(
            ~measure,
            "CasesTotal",
            "DeathsTotal",
            "TestsTotal"
        ) %>%
            dplyr::mutate(
                measureShort = stringr::str_remove(measure, "Total"),
                previousName = glue::glue("{measure}Previous"),
                previousCalc = glue::glue("dplyr::lag({measure}, n = 1, order_by = Date)"),
                newName = glue::glue("{measureShort}NewRaw"),
                newCalc = glue::glue("{measure} - {previousName}"),
                newPer100KName = glue::glue("{measureShort}NewPer100K"),
                newPer100KCalc = glue::glue("{newName} / Population * 100000")
            )

        #Split up the data by GeoID
        df_split <- tibble::tibble(GeoID = unique(df[["GeoID"]]),
                                   data = purrr::map(GeoID, ~df[df$GeoID == .x,])
        )


        #Conduct the calculations on the formatted data and
        out <- purrr::map_dfr(df_split[["data"]], ~{

            #Select the two days needed for the calculation
            outdf <- .x %>% dplyr::filter(Date <= max(Date) - lagDays) %>%
                dplyr::filter(Date %in% c(max(Date), (max(Date) - days)))

            #Select and create base columns
            outdf <- outdf %>%
                dplyr::select(Jurisdiction, State, Region, GeoID, Date, CasesTotal, DeathsTotal, TestsTotal, Population) %>%
                dplyr::mutate(days = days, lagDays = lagDays)

            #Lag the data with mutate so the different can be calculated in the totals then filter out the most recent record
            outdf <- outdf %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$previousName, mutateCalc = calcTable$previousCalc) %>%
                dplyr::filter(Date == max(Date))

            #Calculate the new and new per capita columns
            outdf <- outdf %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$newName, mutateCalc = calcTable$newCalc) %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$newPer100KName, mutateCalc = calcTable$newPer100KCalc)

            #Remove the unneeded columns and pivot the data into long format
            outdf <- outdf %>%
                dplyr::select(-c(calcTable$measure, calcTable$previousName)) %>%
                tidyr::pivot_longer(data = .,
                                    cols = CasesNewRaw:TestsNewPer100K,
                                    names_to = c("Measure", "Raw_Per100K"),
                                    names_pattern = "(.*)New(.*)",
                                    values_to = "NewValue"
                )

            #Add positive and negative test estimates to the rows where Measure == 'Tests'
            outdf <- outdf %>%
                dplyr::group_by(Raw_Per100K) %>% dplyr::group_split() %>%
                purrr::map_dfr(~{
                    newCases <- .x$NewValue[.x$Measure == 'Cases']
                    newTests <- .x$NewValue[.x$Measure == 'Tests']
                    .x %>% dplyr::mutate(
                        PostitiveTests = dplyr::if_else(Measure == 'Tests', newCases, NA_real_),
                        NegativeTests = dplyr::if_else(Measure == 'Tests', newTests - newCases, NA_real_)
                    )
                })

            #Add Slicer and filterLevels for ease of selection and filtering in Power Bi
            outdf <- outdf %>%
                dplyr::mutate(
                    SlicerLevels = glue::glue("Last {days} days{dplyr::if_else(lagDays == 0, '.', paste0(' given a ', lagDays, ' day lag.'))}"),
                    filterLevels = glue::glue("{stringr::str_pad(days, 2, pad = '0')}_{stringr::str_pad(lagDays, 2, pad = '0')}")
                )

            return(outdf)

        })
    } else {

        #Do the Calculations for the 'All Time' summary

        #Select and create base columns and filter for the most recent data
        out <- df %>%
            dplyr::select(Jurisdiction, State, Region, GeoID, Date, Population, CasesTotal, DeathsTotal, TestsTotal) %>%
            marcR::groupby_rank(GeoID, rankby = Date, filterIDs = 1) %>%
            dplyr::mutate(CasesPer100K = CasesTotal / Population * 100000,
                          DeathsPer100K = DeathsTotal / Population * 100000,
                          TestsPer100K = TestsTotal / Population * 100000)

        #Pivot the data into long format and fix the factors in Raw_Per100K
        out <- out %>%
            tidyr::pivot_longer(data = .,
                                cols = CasesTotal:TestsPer100K,
                                names_to = c("Measure", "Raw_Per100K"),
                                names_pattern = "(Cases|Deaths|Tests)(.*)",
                                values_to = "NewValue"
            ) %>%
            dplyr::mutate(Raw_Per100K = dplyr::if_else(Raw_Per100K == "Total", "Raw", Raw_Per100K))

        #Add positive and negative test estimates to the rows where Measure == 'Tests'
        out <- out %>%
            dplyr::group_by(GeoID, Raw_Per100K) %>% dplyr::group_split() %>%
            purrr::map_dfr(~{
                newCases <- .x$NewValue[.x$Measure == 'Cases']
                newTests <- .x$NewValue[.x$Measure == 'Tests']
                .x %>% dplyr::mutate(
                    PostitiveTests = dplyr::if_else(Measure == 'Tests', newCases, NA_real_),
                    NegativeTests = dplyr::if_else(Measure == 'Tests', newTests - newCases, NA_real_)
                )
            })

        #Add Slicer and filterLevels for ease of selection and filtering in Power Bi and the days and lagDays columns
        out <- out %>%
            dplyr::mutate(days = NA_real_,
                          lagDays = NA_real_,
                          SlicerLevels = "All Time",
                          filterLevels = "9999")


    }

    return(out)

}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++