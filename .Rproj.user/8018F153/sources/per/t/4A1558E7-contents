#' Data import
#'
#' Imports data to be used in GIS_Tool functions
#' @param GISData Tables of gradient polygons
#' @return list of gradient tables in global environment
#' @export
getdata <- function() {
  dir <- getwd()
  filenames <- list.files(dir)

  result_names <- structure(
    .Data = gsub(
      pattern = "\\.csv",
      replacement = "",
      x = filenames),
    .Names = filenames)


  for (.file in filenames) {
    assign(
      x = result_names[.file],
      value = read.csv(
        file = file.path(dir, .file),
        header = TRUE),
      envir = .GlobalEnv)
  }
}

