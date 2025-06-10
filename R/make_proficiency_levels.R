#' Append a Binary Factor for Student Proficiency
#' 
#' @title Get Proficiency
#' @description Appends a student proficiency data based on the achievement level and the number of achievement levels that are considered proficient
#' 
#' @param dataset A data frame that includes student achievement levels
#' @param n_proficiencies An integer for the number of achievement levels that are considered proficient
#' @param print_table Prints a table of proportions of students who are proficient and not proficient (default: TRUE)
#' @param make_plot Makes a relative frequency bar plot of students who are proficient and not proficient (default: TRUE)
#' 
#' @return A data frame with an appended column containing student proficiency data
#' @importFrom stats aggregate setNames
#' @importFrom graphics barplot
#' @export
#' 
#' @examples
#' make_proficiency_levels(dataset = math, n_proficiencies = 2)
#' 

make_proficiency_levels = function(dataset, n_proficiencies, print_table = TRUE, make_plot = TRUE) {
  # Validate inputs
  if(!is.numeric(n_proficiencies) || floor(n_proficiencies) != n_proficiencies) {
    stop("The number of proficiency levels must be a whole number.")
  }
  
  # Validate and standardize column names
  colnames_lower = tolower(names(dataset))
  
  # Required variables
  required_variables = c("achievement_level")
  
  # Missing variables
  missing_variables = required_variables[!required_variables %in% colnames_lower]
  
  # Check to make sure all of the required variables are included
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }

  # Convert to data frame if not already
  OUT = as.data.frame(dataset)
  
  # Map the original column names
  name_map = setNames(names(dataset), colnames_lower)
  achievement = name_map["achievement_level"]
  
  # Prepare variables
  ACHIEVEMENTS = OUT[[achievement]]

  if(!is.factor(ACHIEVEMENTS)) {
    ACHIEVEMENTS = factor(ACHIEVEMENTS)
  }
  
  # Identify the levels of achievements
  LEVELS = levels(ACHIEVEMENTS)
  
  # Number of achievement levels
  N_LEVELS = length(LEVELS)
  
  # Error Detection
  if(N_LEVELS < n_proficiencies) {
    stop("The number of proficiency levels cannot exceed the number of achievement levels.")
  }
  
  # Binary conversion to proficiency
  TOP_LEVELS = LEVELS[(N_LEVELS - n_proficiencies + 1):N_LEVELS]
  BINARY = ifelse(is.na(ACHIEVEMENTS), NA, 
                  ACHIEVEMENTS %in% TOP_LEVELS)
  PROFICIENCY = factor(BINARY, levels = c(FALSE, TRUE), labels = c("Not Proficient", "Proficient"))
  
  # Append to the dataset
  OUT$PROFICIENCY_LEVELS = PROFICIENCY
  
  # Optional outputs
  if(print_table) {
    print(round(prop.table(table(PROFICIENCY)), 3))
  }
  
  if(make_plot) {
    barplot(prop.table(table(PROFICIENCY)), main = "Proficiency Distribution",
            ylab = "Proportion", col = c("lightgray", "steelblue"))
  }
  
  # Return the new data frame
  return(OUT)
}
