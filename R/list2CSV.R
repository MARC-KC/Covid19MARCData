

#' @export
list2CSV <- function(objectList, outputFolder) {
    message(crayon::green("Exporting", length(objectList), paste0("csv files to '", outputFolder, "\n")))
    if (dir.exists(outputFolder)) unlink(outputFolder, recursive = TRUE) #if folder exists, delete it
    dir.create(outputFolder, recursive = TRUE, showWarnings = FALSE) #create the directory
    purrr::walk(seq_along(objectList), ~{
        fileName <- stringr::str_remove(names(objectList)[.x], "^bi_")
        message(crayon::blue("Saving file:", paste0(fileName, ".csv"), paste0("(", .x, "/", length(objectList), ")\n")))
        readr::write_csv(x = objectList[[.x]], file = file.path(outputFolder, paste0(fileName, ".csv")), na = "")
    })
    message(crayon::green("Export Completed Successfully\n"))
}




