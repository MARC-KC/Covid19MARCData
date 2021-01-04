
if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package \"readr\" needed for this function to work. Please install it.",
         call. = FALSE)
}

if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package \"here\" needed for this function to work. Please install it.",
         call. = FALSE)
}



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Jurisdiction Tables ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Load in Jurisdiction Table
jurisdictionTable <- readr::read_csv(here::here("data-raw", "CovidJurisdictions.csv"), col_types = "ccccilc")

#Filter for CDT data
jurisdictionTableCDT <- jurisdictionTable %>%
    dplyr::filter(CDT_Jurisdiction)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GeoID Filters ####
# These can change as MARC sharing permissions change
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

GeoIDs <- NULL

#all MARC and HCC
GeoIDs[['base']] <- jurisdictionTable %>% dplyr::filter(dplyr::across(Region, ~grepl('MARC|HCC', .))) %>% dplyr::pull(GeoID)

#all MARC
GeoIDs[['MARC']] <- jurisdictionTable %>% dplyr::filter(dplyr::across(Region, ~grepl('MARC', .))) %>% dplyr::pull(GeoID)

#all HCC
GeoIDs[['HCC']] <- jurisdictionTable %>% dplyr::filter(dplyr::across(Region, ~grepl('HCC', .))) %>% dplyr::pull(GeoID)

#Jurisdictions with <=2 hospitals that we don't have permission to share publicly.
GeoIDs[['restrictHospital']] <- GeoIDs[['base']][GeoIDs[['base']] %in% c('29165', '29165NoKC', '29037', '29037NoKC', stringr::str_subset(GeoIDs[['HCC']], "HCC|29177", negate = TRUE))]

#Jurisdictions with hospitals that we have permission to share publicly.
GeoIDs[['okayHospital']] <- GeoIDs[['base']][!(GeoIDs[['base']]  %in% GeoIDs[['restrictHospital']])]

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export Data for Package Use ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
usethis::use_data(jurisdictionTable, jurisdictionTableCDT,
                  GeoIDs,
                  overwrite = TRUE)
#make sure documentation for changes is up to date in /R/data.R
#then document and rebuild package
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
