
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
# Export Data for Package Use ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
usethis::use_data(jurisdictionTable, jurisdictionTableCDT,
                  overwrite = TRUE)
#make sure documentation for changes is up to date in /R/data.R
#then document and rebuild package
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
