#' @title COVID-19 Jurisdiction Information
#'
#' @description A dataset containing keys for jurisdiction information used on the COVID-19 project.
#'
#' @format A data frame with 30 rows and 7 variables:
#' \describe{
#'   \item{Jurisdiction}{Name of the county, city or region; character; [NVARCHAR](30)}
#'   \item{Region}{Name of the region that the jurisdiciton is apart of; character: one of 'HCC', 'MARC', 'State', 'StateMARC', 'MARC_HCC', 'HCCRegion'; [NVARCHAR](10)}
#'   \item{State}{Name of the state that the jurisdiciton is apart of; character: one of 'KS', 'MO', NA; [NVARCHAR](2)}
#'   \item{GeoID}{Unique GeoID code for the jurisdiciton (unique identifier); character; [NVARCHAR](10)}
#'   \item{Population}{Population in the jurisdiciton (source: ACS data); integer; [INT]}
#'   \item{CDT_Jurisdiction}{TRUE/FALSE on if this Jurisdiciton is used to enter Cases, Deaths, Tests (CDT) data; logical; [BIT]}
#'   \item{DisplayName}{Pretty jurisdiction name used for display purposes; character; [NVARCHAR](30)}
#' }
#'
#' @details \code{jurisdictionTableCDT} is just a subset of \code{jurisdictionTable} filtered to where 'CDT_Jurisdiction' is TRUE
#'
#' @source '/data-raw/CovidJurisdictions.csv'
#' @rdname jurisTable
"jurisdictionTable"

#' @rdname jurisTable
"jurisdictionTableCDT"


#' @title GeoID Filters
#'
#' @description A named list containing different GeoID subsets based on jurisdictionTable.
#'
#' @format A named list:
#' \describe{
#'   \item{base}{Contains all MARC and all HCC GeoID's}
#'   \item{MARC}{Contains all MARC GeoID's}
#'   \item{HCC}{Contains all HCC GeoID's}
#'   \item{restrictHospital}{Contains all restricted GeoID's due to lack of permission from hosptial to display their data. We have to have explicit permission from all hospitals to display data in those that have <=2 hospitals}
#'   \item{okayHospital}{Contains all non-restricted GeoID's; Inverse of restrictHospital}
#' }
#' @source '/data-raw/CovidJurisdictions.csv'
"GeoIDs"
