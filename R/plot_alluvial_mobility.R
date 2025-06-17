#' Plot an Alluvial Diagram of Student Mobility
#'
#' @title Plot Alluvial Mobility
#' @description Creates an alluvial diagram that illustrates student mobility between years, segmented by gender and ethnicity. If `MOBILITY_STATUS` is not already present in the dataset, it will be computed using the provided grade and year.
#'
#' @param dataset A data frame that includes student grade level, academic year, gender, and ethnicity.
#' @param current_year The academic year to evaluate (e.g., "2020_2021").
#' @param current_grade The grade level to evaluate (e.g., 4 or "4").
#' @param print_plot Logical; if TRUE (default), the alluvial plot is printed.
#'
#' @return A list containing:
#' \describe{
#'   \item{Data}{The updated dataset used for plotting}
#'   \item{Data_Table}{A contingency table of Mobility x Gender x Ethnicity}
#'   \item{Table_by_Ethnicity}{Proportion table by ethnicity}
#'   \item{Table_by_Gender}{Proportion table by gender}
#'   \item{Caption}{Text summary of transition between grades}
#'   \item{Plot}{The generated ggplot2 object}
#' }
#'
#' @import ggplot2
#' @importFrom ggalluvial geom_alluvium geom_stratum StatStratum
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' plot_alluvial_mobility(dataset = math, current_year = "2020_2021", current_grade = 4)
#' 

plot_alluvial_mobility <- function(dataset, current_year, current_grade, print_plot = TRUE) {
  # Normalize current grade input
  numeric_grade <- normalize_grade(current_grade)
  
  # Standardize column names to lower case
  colnames_lower <- tolower(names(dataset))
  
  # If Mobility_Status is missing, generate it using make_mobility()
  if (!"mobility_status" %in% colnames_lower) {
    dataset <- make_mobility(dataset, current_year, current_grade, print_plot = FALSE)$Data
  }
  
  # Check for GENDER and ETHNICITY variables (handling cases like GENDER.x/y from joins)
  colnames_lower <- tolower(names(dataset))
  if (!"gender" %in% colnames_lower || !"ethnicity" %in% colnames_lower) {
    dataset$GENDER <- factor(
      ifelse(!is.na(dataset$GENDER.x), dataset$GENDER.x, dataset$GENDER.y),
      levels = unique(na.omit(c(dataset$GENDER.x, dataset$GENDER.y)))
    )
    dataset$ETHNICITY <- factor(
      ifelse(!is.na(dataset$ETHNICITY.x), dataset$ETHNICITY.x, dataset$ETHNICITY.y),
      levels = unique(na.omit(c(dataset$ETHNICITY.x, dataset$ETHNICITY.y)))
    )
  }
  
  # Generate academic year text strings
  last_year_grade <- numeric_grade - 1
  last_year_grade <- ifelse(last_year_grade == 0, "K",
                            ifelse(last_year_grade == -1, "PreK", last_year_grade))
  last_year_text <- paste0(as.numeric(substr(current_year, 1, 4)) - 1, "-", 
                           as.numeric(substr(current_year, 1, 4)))
  current_year_text <- paste0(as.numeric(substr(current_year, 1, 4)), "-", 
                              as.numeric(substr(current_year, 1, 4)) + 1)
  
  # Construct alluvial plot data from contingency table
  TABLE <- table(dataset$MOBILITY_STATUS, dataset$GENDER, dataset$ETHNICITY)
  DATAFRAME <- as.data.frame(TABLE)
  names(DATAFRAME) <- c("Mobility", "Gender", "Ethnicity", "Frequency")
  
  # Create the alluvial plot
  PLOT1 <- ggplot(DATAFRAME, aes(axis1 = Mobility, axis2 = Gender, axis3 = Ethnicity, y = Frequency)) +
    geom_alluvium(aes(fill = Mobility)) +
    geom_stratum() +
    geom_text(stat = ggalluvial::StatStratum, aes(label = after_stat(stratum))) +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste("Grade", current_grade, "in", current_year_text))
  
  # Create proportion tables for additional insights
  mobility_ethnicity <- round(prop.table(table(dataset$MOBILITY_STATUS, dataset$ETHNICITY), 
                                         margin = 2), 3) * 100
  mobility_ethnicity <- apply(mobility_ethnicity, c(1, 2), function(x) sprintf("%.1f%%", x))
  
  mobility_gender <- round(prop.table(table(dataset$MOBILITY_STATUS, dataset$GENDER), 
                                      margin = 2), 3) * 100
  mobility_gender <- apply(mobility_gender, c(1, 2), function(x) sprintf("%.1f%%", x))
  
  # Prepare output object
  OUT <- list()
  OUT$Data <- dataset
  OUT$Data_Table <- TABLE
  OUT$Table_by_Ethnicity <- mobility_ethnicity
  OUT$Table_by_Gender <- mobility_gender
  OUT$Caption <- paste(
    "Demographics of Students from Grade", last_year_grade, "in", last_year_text,
    "to Grade", current_grade, "in", current_year_text
  )
  OUT$Plot <- PLOT1
  
  if (print_plot) print(PLOT1)
  
  return(invisible(OUT))
}

