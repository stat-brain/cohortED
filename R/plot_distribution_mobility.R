#' Plot Mobility Distribution over Grade Levels
#' 
#' @title Plot Mobility Distribution
#' @description Creates an Alluvial Diagram depicting Student Mobility from previous year
#' 
#' @param dataset A data frame that includes student grade level and academic year
#' @param year The academic year to begin evaluating with (only required if not already used make_mobility)
#' @param start_grade (Optional)
#' @param end_grade (Optional)
#' @param print_table Prints a contingency table of student mobility by grade level (default: TRUE)
#' @param data_out Outputs a data frame containing the student mobility data
#' 
#' @return The Distribution of Student Mobility for a given Year for all Grade levels
#' @import ggplot2
#' @export
#' 

plot_distribution_mobility = function(dataset, year, start_grade = NULL, end_grade = NULL, print_table = TRUE, data_out = FALSE) {
  # Validate and standardize column names
  colnames_lower = tolower(names(dataset))
  
  # Required variables
  required_variables = c("id", "year")
  
  # Missing variables
  missing_variables = required_variables[!required_variables %in% colnames_lower]
  
  # Check to make sure all of the required variables are included
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Map the original column names
  name_map = setNames(names(dataset), colnames_lower)
  id_col = name_map["id"]
  grade_col = name_map["grade"]
  year_col = name_map["year"]
  
  # Convert to data frame if not already
  NEW = as.data.frame(dataset)
  
  # Remove any students without grade level or academic year information
  NEW = NEW[!is.na(NEW[[grade_col]]) & !is.na(NEW[[year_col]]), ]
  
  # Extract the beginning of the year from within the academic year
  NEW$NUMERIC_YEAR = as.numeric(substr(NEW$YEAR, 1, 4))
  if(is.character(start_year)) {
    last_year = as.numeric(substr(start_year, 1, 4)) - 1
  }
  
  # Make sure year and grade are comparable (convert to numeric if needed)
  NEW[[grade_col]] = as.numeric(NEW[[grade_col]])
  
  if(is.null(start_grade)) {
    start_grade = 2
  }
  
  if(is.null(end_grade)) {
    end_grade = 12
  }
  
  start_grade = as.numeric(start_grade)
  end_grade = as.numeric(end_grade)
  
  GRADE_SEQUENCE = seq(from = start_grade, to = end_grade, by = 1) - 1
  
  for(i in GRADE_SEQUENCE) {
    
  }
}