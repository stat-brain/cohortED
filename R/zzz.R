#' @importFrom utils globalVariables

.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c("Mobility", "Gender", "Ethnicity", "Frequency", "stratum"))
}