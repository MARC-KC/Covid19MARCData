







#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FNs - mostRecentGivenTime - Creates data formatted bar charts with time slicers on CDT pages####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#' @export
mostRecentGivenTime <- function(df, days, lagDays) {

    if (!is.na(days)) {


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

        df_split <- tibble::tibble(GeoID = unique(df[["GeoID"]]),
                                   data = purrr::map(GeoID, ~df[df$GeoID == .x,])
        )

        # .x <- df_split[["data"]][[16]]
        # .x

        out <- purrr::map_dfr(df_split[["data"]], ~{

            .x %>% dplyr::filter(Date <= max(Date) - lagDays) %>%
                dplyr::filter(Date %in% c(max(Date), (max(Date) - days))) %>%
                dplyr::select(Jurisdiction, State, Region, GeoID, Date, CasesTotal, DeathsTotal, TestsTotal, Population) %>%
                dplyr::mutate(days = days, lagDays = lagDays) %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$previousName, mutateCalc = calcTable$previousCalc) %>%
                dplyr::filter(Date == max(Date)) %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$newName, mutateCalc = calcTable$newCalc) %>%
                marcR::mutateCalcString(df = ., mutateName = calcTable$newPer100KName, mutateCalc = calcTable$newPer100KCalc) %>%
                dplyr::select(-c(calcTable$measure, calcTable$previousName)) %>%
                tidyr::pivot_longer(data = .,
                                    cols = CasesNewRaw:TestsNewPer100K,
                                    names_to = c("Measure", "Raw_Per100K"),
                                    names_pattern = "(.*)New(.*)",
                                    values_to = "NewValue"
                ) %>%
                dplyr::group_by(Raw_Per100K) %>% dplyr::group_split() %>%
                purrr::map(~{
                    newCases <- .x$NewValue[.x$Measure == 'Cases']
                    newTests <- .x$NewValue[.x$Measure == 'Tests']
                    .x %>% dplyr::mutate(
                        PostitiveTests = dplyr::if_else(Measure == 'Tests', newCases, NA_real_),
                        NegativeTests = dplyr::if_else(Measure == 'Tests', newTests - newCases, NA_real_)
                    )
                }) %>% dplyr::bind_rows() %>%
                dplyr::mutate(
                    SlicerLevels = glue::glue("Last {days} days{dplyr::if_else(lagDays == 0, '.', paste0(' given a ', lagDays, ' day lag.'))}"),
                    filterLevels = glue::glue("{stringr::str_pad(days, 2, pad = '0')}_{stringr::str_pad(lagDays, 2, pad = '0')}")
                )

        })
    } else {

        out <- df %>%
            dplyr::select(Jurisdiction, State, Region, GeoID, Date, Population, CasesTotal, DeathsTotal, TestsTotal) %>%
            dplyr::mutate(CasesPer100K = CasesTotal / Population * 100000,
                          DeathsPer100K = DeathsTotal / Population * 100000,
                          TestsPer100K = TestsTotal / Population * 100000) %>%
            tidyr::pivot_longer(data = .,
                                cols = CasesTotal:TestsPer100K,
                                names_to = c("Measure", "Raw_Per100K"),
                                names_pattern = "(Cases|Deaths|Tests)(.*)",
                                values_to = "NewValue"
            ) %>%
            dplyr::mutate(Raw_Per100K = dplyr::if_else(Raw_Per100K == "Total", "Raw", Raw_Per100K)) %>%
            dplyr::group_by(GeoID, Raw_Per100K) %>% dplyr::group_split() %>%
            purrr::map_dfr(~{
                newCases <- .x$NewValue[.x$Measure == 'Cases']
                newTests <- .x$NewValue[.x$Measure == 'Tests']
                .x %>% dplyr::mutate(
                    PostitiveTests = dplyr::if_else(Measure == 'Tests', newCases, NA_real_),
                    NegativeTests = dplyr::if_else(Measure == 'Tests', newTests - newCases, NA_real_)
                )
            }) %>%
            dplyr::mutate(days = NA_real_,
                          lagDays = NA_real_,
                          SlicerLevels = "All Time",
                          filterLevels = "9999")


    }

    return(out)

}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
