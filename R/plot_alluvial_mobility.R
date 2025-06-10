#' Plot an Alluvial Diagram of Student Mobility
#' 
#' @title Plot Alluvial Mobility
#' @description Creates an Alluvial Diagram depicting Student Mobility from previous year
#' 
#' @param dataset A data frame that includes student grade level and academic year
#' @param start_year The academic year to begin evaluating with
#' @param start_grade The grade level to begin evaluating with
#' @param print_table Prints a contingency table of indicators associated with student mobility (default: TRUE)
#' @param make_plot Makes an alluvial diagram depicting student mobility (default: TRUE)
#' 
#' 
#' @return A table of proportions of students
#' @import ggplot2
#' @importFrom ggalluvial geom_alluvium geom_stratum geom_flow
#' @importFrom stats aggregate setNames
#' @export
#' 
#'
#' 

plot_alluvial_mobility = function(dataset, start_year, start_grade, print_table = TRUE, make_plot = TRUE) {
  
  
}