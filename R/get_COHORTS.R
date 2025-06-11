#' Split Long Dataset into Cohorts by Grade and Year
#'
#' This function takes a long-format dataset with grade and year information 
#' and splits it into a list of cohort-specific datasets.
#'
#' @param dataset A data frame in long format with at least two columns: `GRADE` and `YEAR`.
#'
#' @return A list of data frames, each corresponding to a unique grade-year combination (cohort).
#' @importFrom stats aggregate setNames
#' @export
#' 
#' @examples
#' get_cohorts(dataset = math)
#' 

get_cohorts = function(dataset) {
  # Input validation
  if(!is.data.frame(dataset)) {
    stop("'dataset' must be a data frame.")
  }
  
  # Validate and standardize column names
  colnames_lower = tolower(names(dataset))
  
  # Required variables
  required_variables = c("grade", "year")
  
  # Missing variables
  missing_variables = required_variables[!required_variables %in% colnames_lower]
  
  # Check to make sure all of the required variables are included
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Map original column names to lowercase
  col_map = setNames(names(dataset), tolower(names(dataset)))
  
  grade_col = col_map["grade"]
  year_col = col_map["year"]
  
  # Extract grade and year using original column names
  dataset$COHORT_ID = paste0("Grade_", dataset[[grade_col]], ".", dataset[[year_col]])
  
  COHORTS = split(dataset, dataset$COHORT_ID, drop = TRUE)
  
  # Return the split up dataset
  return(COHORTS)
}