#' Append a Column for Grade Level and Year indicating when the Student first appeared in the Dataset
#' 
#' @title Get First Entered
#' @description Appends a column of "FIRST_ENTERED" to the dataset to indicate which cohort they were initially included in
#' 
#' @param dataset A data frame that includes student grade level and academic year
#' 
#' @return A data frame with an appended column containing student entry cohort data
#' @importFrom stats aggregate setNames
#' @export
#' 
#' @examples
#' make_first_entered(dataset = math)
#' 

make_first_entered = function(dataset) {
  # Validate and standardize column names
  colnames_lower = tolower(names(dataset))
  
  # Required variables
  required_variables = c("id", "grade", "year")
  
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
  OUT = as.data.frame(dataset)
  
  # Ensure GRADE and YEAR are characters for safe concatenation
  OUT[[grade_col]] = as.character(OUT[[grade_col]])
  OUT[[year_col]] = as.character(OUT[[year_col]])
  
  # Extract the start year from within the academic year
  OUT$START_YEAR = as.numeric(substr(OUT$YEAR, 1, 4))
  
  # Prepare variables
  OUT = OUT[order(OUT[[id_col]], OUT$START_YEAR), ]
  
  temporary = OUT[ , c(id_col, grade_col, year_col)]
  names(temporary) = c("ID", "GRADE", "YEAR")
  
  # Extract the desired information
  FIRST_INFO = aggregate(cbind(GRADE, YEAR) ~ ID, data = temporary, FUN = function(x) x[1])
  
  # Rename the ID column in FIRST_INFO
  names(FIRST_INFO)[names(FIRST_INFO) == "ID"] = id_col
  
  # Format as GRADE.YEAR
  FIRST_INFO$FIRST_ENTERED = paste(FIRST_INFO$GRADE, FIRST_INFO$YEAR, sep = ".")
  
  # Append to the original dataset
  OUT = merge(dataset, FIRST_INFO[ , c(id_col, "FIRST_ENTERED")], by = id_col, all.x = TRUE)
  OUT = as.data.frame(OUT)
  
  # Return the new data frame
  return(OUT)
}
