
#' @title Export List to CSVs
#'
#' @param dfList A named list of data.frames for export
#' @param outputFolder The file path to the folder that the CSV's will be exported to. Defaults to \code{file.path(getwd(), 'outputDatasetsForPowerBi')}
#' @param overwriteDirectory Should the directory be deleted and recreated? Defaults to FALSE as this is a dangerous option
#'
#' @export
list2CSV <- function(dfList, outputFolder = file.path(getwd(), 'outputDatasetsForPowerBi'), overwriteDirectory = FALSE) {
    message(crayon::yellow("Exporting", length(dfList), paste0("csv files to '", outputFolder)))
    if (dir.exists(outputFolder) & overwriteDirectory) unlink(outputFolder, recursive = TRUE) #if folder exists, delete it
    dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE) #create the directory
    purrr::walk(seq_along(dfList), ~{
        fileName <- stringr::str_remove(names(dfList)[.x], "^bi_")
        message(crayon::blue("Saving file:", paste0(fileName, ".csv"), paste0("(", .x, "/", length(dfList), ")")))
        readr::write_csv(x = dfList[[.x]], file = file.path(outputFolder, paste0(fileName, ".csv")), na = "")
    })
    message(crayon::green("Export Completed Successfully"))
}




