#' Takes a long dataset and separates it into a list of cohort datasets by grade level and year
#' 
#' @title Get Cohort Data from Long Dataset
#' @description Returns "Hello, World!" in various languages using the Hello_World dataset
#' 
#' @param DATASET a data frame in long formate with columns for GRADE and YEAR
#' 
#' @return A list containing cohort datasets by grade level and year
#' @export
#' 
#' @examples
#' @importFrom SGPdata sgpDATA_LONG
#' get_COHORT(DATASET = sgpDATA_LONG[sgpDATA_LONG$CONTENT_AREA == "MATHEMATICS", ])
#' 



get_COHORTS = function(DATASET) {
  COHORTS = split(DATASET, list(DATASET$GRADE, DATASET$YEAR), drop = TRUE)
  return(COHORTS)
}