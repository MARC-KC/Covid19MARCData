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
#' }
#'
#' @details \code{jurisTable} and \code{popTable} are just a subsets of \code{Covid19MARCInternal::jurisdictionTable} with specific columns filtered
#'
#' @source 'Covid19MARCInternal::jurisdictionTable'
#' @rdname jurisTable
"jurisTable"

#' @rdname jurisTable
"popTable"


#' @title GeoID Filters
#'
#' @description A named list containing different GeoID subsets based on the jurisdictionTable in Covid19MARCInternal.
#'
#' @format A named list:
#' \describe{
#'   \item{base}{Contains all MARC and all HCC GeoID's}
#'   \item{MARC}{Contains all MARC GeoID's}
#'   \item{HCC}{Contains all HCC GeoID's}
#'   \item{restrictHospital}{Contains all restricted GeoID's due to lack of permission from hosptial to display their data. We have to have explicit permission from all hospitals to display data in those that have <=2 hospitals}
#'   \item{okayHospital}{Contains all non-restricted GeoID's; Inverse of restrictHospital}
#' }
#' @source 'Covid19MARCInternal::GeoIDs'
"GeoIDs"


#' @title Pretty Jurisdictions
#'
#' @description This dataset is a pure Power Bi helper for making slicers and
#'   jurisdiciton ordering
#'
#' @format
#' \describe{
#'   \item{Site}{Is this for the 'MARC' or the 'HCC' Hub?}
#'   \item{GeoID}{Unique GeoID code for the jurisdiciton (unique identifier)}
#'   \item{NiceName}{Formatted jurisdiciton name}
#'   \item{DisplayOrder}{The order for display of the Jurisdictions (larger combined regions first)}
#'   \item{NiceUpperLevel}{Formatted overarching category for the jurisdicion}
#'   \item{ICUBedsDisclaimer}{Adds a 'Not Reported' card instead of displaying a blank figure for jurisdicitons that are not reporting ICU beds (Not sure this is used anymore)}
#'   \item{SourceLocation}{URL for the data source.}
#' }
#' @source '/data-raw/prettyJurisdictions.csv'
"prettyJurisdictions"
