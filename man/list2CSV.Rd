% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list2CSV.R
\name{list2CSV}
\alias{list2CSV}
\title{Export List to CSVs}
\usage{
list2CSV(
  dfList,
  outputFolder = file.path(getwd(), "outputDatasetsForPowerBi"),
  overwriteDirectory = FALSE
)
}
\arguments{
\item{dfList}{A named list of data.frames for export}

\item{outputFolder}{The file path to the folder that the CSV's will be exported to. Defaults to \code{file.path(getwd(), 'outputDatasetsForPowerBi')}}

\item{overwriteDirectory}{Should the directory be deleted and recreated? Defaults to FALSE as this is a dangerous option}
}
\description{
Export List to CSVs
}
