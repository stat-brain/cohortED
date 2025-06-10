#' Append a Factor for Student Mobility
#' 
#' @title Make Mobility
#' @description Appends a year-to-year student mobility category based on a given starting year
#' 
#' @param dataset A data frame that includes student grade level and academic year
#' @param start_year The academic year to begin evaluating with
#' @param start_grade The grade level to begin evaluating with
#' @param print_table Prints a table of proportions of students for student mobility (default: TRUE)
#' @param make_plot Makes a relative frequency bar plot of student mobility (default: TRUE)
#' 
#' 
#' @return A table of proportions of students and an alluvial 
#' @importFrom stats aggregate setNames
#' @importFrom graphics barplot
#' @export
#' 
#' @examples
#' make_mobility(dataset = math, start_year = "2019_2020", start_grade = 3)
#' make_mobility(dataset = math, start_year = 2021, start_grade = 6, print_table = FALSE)
#' 

make_mobility = function(dataset, start_year, start_grade, print_table = TRUE, make_plot = TRUE) {
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
  NEW = as.data.frame(dataset)
  
  # Remove any students without grade level or academic year information
  NEW = NEW[!is.na(NEW[[grade_col]]) & !is.na(NEW[[year_col]]), ]
  
  # Extract the beginning of the year from within the academic year
  NEW$NUMERIC_YEAR = as.numeric(substr(NEW$YEAR, 1, 4))
  if(is.character(start_year)) {
    start_year = as.numeric(substr(start_year, 1, 4))
  }
  
  # Make sure year and grade are comparable (convert to numeric if needed)
  NEW[[grade_col]] = as.numeric(NEW[[grade_col]])
  start_grade = as.numeric(start_grade)
  
  # Subset rows
  START = NEW[NEW$NUMERIC_YEAR == start_year & NEW[[grade_col]] == start_grade, ]
  END = NEW[NEW$NUMERIC_YEAR == (start_year + 1) & NEW[[grade_col]] == (start_grade + 1), ]
  
  # Remove the Extra Column
  START$NUMERIC_YEAR = NULL
  END$NUMERIC_YEAR = NULL
  
  # Vector of IDs
  START_ID = unique(START[[id_col]])
  END_ID = unique(END[[id_col]])
  
  # All unique IDs
  ALL_ID = union(START_ID, END_ID)
  
  # Determine mobility status
  MOBILITY = ifelse(ALL_ID %in% START_ID & ALL_ID %in% END_ID, "Stay", 
                    ifelse(ALL_ID %in% START_ID, "Leave", "Join"))
  MOBILITY = factor(MOBILITY, levels = c("Leave", "Stay", "Join"))
  
  # Combine this vector with all ids
  MOBILITY = data.frame(ID = ALL_ID, MOBILITY_STATUS = MOBILITY)
  
  # Create a merged data set
  OUT = merge(x = START, y = END, by = "ID", all = TRUE)
  OUT = merge(x = OUT, y = MOBILITY, by = "ID", all = TRUE)
  
  # Optional outputs
  if(print_table) {
    print(round(prop.table(table(OUT$MOBILITY_STATUS)), 3))
  }
  
  if(make_plot) {
    barplot(prop.table(table(OUT$MOBILITY_STATUS)), main = "Mobility Distribution",
            ylab = "Proportion", col = c("darkgray", "steelblue", "lightgray"))
  }

  # Output the desired data set
  return(OUT)
}
