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
    dplyr::mutate(CovidNew24H = CovidNew24HConfirmed + CovidNew24HSuspected) %>%
    dplyr::select(
        Jurisdiction, State, GeoID, Region, Date,

        CasesNew, CasesTotal,
        DeathsNew, DeathsTotal,
        TestsNew, TestsTotal,
        Population,

        HospitalsReporting, HospitalsTotal,
        BedsTotal, BedsUsed, BedsAvailable,
        BedsICUTotal, BedsICUUsed, BedsICUAvailable,
        CovidTotal, CovidConfirmed, CovidSuspected,
        CovidNew24H, CovidNew24HConfirmed, CovidNew24HSuspected,
        CovidICUTotal, CovidICUConfirmed, CovidICUSuspected,
        VentilatorsTotal, VentilatorsUsed, VentilatorsAvailable,
        CovidVentilatorsUsed)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
