#' Plot an Alluvial Diagram of Student Mobility
#' 
#' @title Plot Alluvial Mobility
#' @description Creates an Alluvial Diagram depicting Student Mobility from previous year
#' 
#' @param dataset A data frame that includes student grade level and academic year
#' @param start_year The academic year to begin evaluating with (only required if not already used make_mobility)
#' @param start_grade The grade level to begin evaluating with (only required if not already used make_mobility)
#' @param print_table Prints a contingency table of indicators associated with student mobility (default: TRUE)
#' @param data_out Returns an updated data frame with merged gender and ethnicity columns (default: FALSE)
#' 
#' 
#' @return An Alluvial Diagram showing Gender, Ethnicity, and Mobility with Optional Contingency Table
#' @import ggplot2
#' @importFrom ggalluvial geom_alluvium geom_stratum stat_stratum
#' @export
#' 
#' @examples
#' plot_alluvial_mobility(dataset = math, 
#'   start_year = "2019_2020", start_grade = 3, print_table = FALSE, data_out = FALSE)
#' 

plot_alluvial_mobility = function(dataset, start_year, start_grade, print_table = TRUE, data_out = FALSE) {
  # Check to see if the dataset currently contains Mobility_Status as column
  colnames_lower = tolower(names(dataset))
  
  # Required variables
  required_variables = c("mobility_status")
  
  # Missing variables
  missing_variables = required_variables[!required_variables %in% colnames_lower]
  
  # Check to make sure all of the required variables are included
  if(length(missing_variables) > 0) {
    dataset = make_mobility(dataset, start_year, start_grade, print_table = FALSE, make_plot = FALSE)
  }
  
  # Required variables
  supported_variables = c("gender", "ethnicity")
  
  # Missing variables
  colnames_lower = tolower(names(dataset))
  missing_variables = supported_variables[!supported_variables %in% colnames_lower]
  
  if(length(missing_variables) > 0) {
    # Coalesce columns
    dataset$GENDER = factor(ifelse(!is.na(dataset$GENDER.x), 
                                  dataset$GENDER.x, dataset$GENDER.y), 
                            labels = levels(dataset$GENDER.x))
    dataset$ETHNICITY = factor(ifelse(!is.na(dataset$ETHNICITY.x), 
                                      dataset$ETHNICITY.x, dataset$ETHNICITY.y), 
                              labels = levels(dataset$ETHNICITY.x))
  }
  
  # Create an alluvial table
  TABLE = table(dataset$MOBILITY_STATUS, dataset$GENDER, dataset$ETHNICITY)
  DATAFRAME = data.frame(TABLE)
  names(DATAFRAME) = c("Mobility", "Gender", "Ethnicity", "Frequency")
  
  # Create the alluvial plot
  GRAPH = ggplot(data = DATAFRAME, aes(axis1 = Mobility, axis2 = Gender, axis3 = Ethnicity, y = Frequency)) + 
    geom_alluvium(aes(fill = Mobility)) + 
    geom_stratum() + 
    geom_text(stat = ggalluvial::StatStratum, aes(label = after_stat(stratum))) + 
    theme_void() + 
    theme(legend.position = "none")
  
  # Print the alluvial plot
  print(GRAPH)
  
  if(print_table) {
    print(round(prop.table(TABLE), 3))
  }
  
  # Return dataset if desired
  return(dataset)
}

