#' Package load hook
#'
#' This function is automatically called when the package is loaded.
#' It registers global variables to avoid R CMD check notes about
#' no visible binding for global variables.
#'
#' @param libname The path to the library where the package is installed.
#' @param pkgname The name of the package.
#' @keywords internal
#' 

.onLoad <- function(libname, pkgname) {
  utils::globalVariables(c(
    "Mobility", "Gender", "Ethnicity", "Frequency", "stratum",
    "ACHIEVEMENT_LEVEL", "PERCENT", "Grade", "Count", "Cohort", "Year",
    "GRADE", "MOBILITY", "% Proficient",
    "Mobility_Status", "Achievement_Level", "Percent",
    # Add these for analyze_cohort_persistence:
    "COUNT", "Cohort_Label", "JOIN_YEAR_LABEL", "JOIN_GRADE",
    "Years_Since_Join", "FillValue", "Label", "LabelColor", "AcademicYear",
    "Cohort_Label_Stack", "YearsSinceJoin", "PercentRetained"
  ))
}
