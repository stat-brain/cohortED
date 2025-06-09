#' Takes a long dataset and separates it into a list of cohort datasets by grade level and year
#' 
#' @title Get Cohort Data from Long Dataset
#' @description Returns "Hello, World!" in various languages using the Hello_World dataset
#' 
#' @param dataset a data frame in long formate with columns for GRADE and YEAR
#' 
#' @return A list containing cohort datasets by grade level and year
#' @export
#' 
#' @examples
#' get_COHORTS(dataset = math)
#' 



get_COHORTS = function(dataset) {
  COHORTS = split(dataset, list(dataset$GRADE, dataset$YEAR), drop = TRUE)
  return(COHORTS)
}