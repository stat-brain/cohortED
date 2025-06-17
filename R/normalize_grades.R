#' Normalize grade levels to numeric values for analysis
#' 
#' Converts grade representations such as "PK", "PreK", and "K" to numeric codes:
#' - "PK" or "PreK" become -1
#' - "K" becomes 0
#' - Numeric grades remain as numeric values
#' 
#' @title Normalize Grades
#' 
#' @param grade A vector of grade values (character or numeric)
#' @return A numeric vector with normalized grade values
#' @export
#' 
#' @examples
#' grades <- c("PK", "PreK", "K", 1, 2, "3", "12")
#' normalize_grade(grades)
#' 

normalize_grade <- function(grade) {
  # Convert to uppercase character for consistent comparison
  grade_char <- toupper(as.character(grade))
  
  # Assign numeric codes based on grade value
  normalized <- ifelse(
    grade_char %in% c("PK", "PREK"), -1,   # Pre-Kindergarten as -1
    ifelse(
      grade_char == "K", 0,                 # Kindergarten as 0
      as.numeric(grade_char)                # Numeric grades remain as is
    )
  )
  
  return(normalized)
}