#' Data import
#'
#' Imports data to be used in GIS_Tool functions
#' @param GISData Tables of gradient polygons
#' @return list of gradient tables in global environment
#' @export
getdata <- function() {
  dir <- getwd()
  filenames <- list.files()

  result_names1 <- structure(
    .Data = gsub(
      pattern = "^..",
      replacement = "WB",
      x = filenames),
    .Names = filenames)

  result_names2 <- structure(
    .Data = gsub(
      pattern = "\\.csv",
      replacement = "",
      x = result_names1),
    .Names = filenames)


  for (.file in filenames) {
    assign(
      x = result_names2[.file],
      value = read.csv(
        file = file.path(dir, .file),
        header = TRUE),
      envir = .GlobalEnv)
  }
}
