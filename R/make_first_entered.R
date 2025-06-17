#' Identify First Grade-Year Entry per Student and Visualize Entry Patterns
#'
#' This function identifies the first grade-year entry per student in a longitudinal dataset,
#' appends a `"FIRST_ENTERED"` column, produces a contingency table of first entries by grade and year,
#' and optionally generates two line plots of entry patterns across grades and years.
#'
#' The function standardizes grade values by converting `"K"` to `0`, and `"PK"` or `"PreK"` to `-1` (case-insensitive),
#' allowing them to be plotted numerically. A totals row and column are added to the contingency table.
#'
#' @title Make First Entered
#' 
#' @param dataset A data frame containing student-level longitudinal data.
#'   Must include columns representing student IDs, grades, and academic years.
#' @param print_plot Logical (default: \code{FALSE}). If `TRUE`, the function will print both plots to the active graphics device.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{`Data`}{The input dataset with a new column `FIRST_ENTERED`, indicating each student's first observed grade and year (e.g., `"1.2019"`).}
#'   \item{`Table`}{A contingency table of first-time student counts by grade and year, with total row and column margins.}
#'   \item{`Caption`}{A string caption describing the contingency table.}
#'   \item{`Plot_by_Grade`}{A `ggplot2` line plot showing number of first-time students by grade and cohort (cohort defined as `year - grade`).}
#'   \item{`Plot_by_Year`}{A `ggplot2` line plot showing first-time student counts by academic year and grade level.}
#' }
#'
#' @details
#' The function assumes the year is in a string format such as `"2019-20"` or `"2019"`, and extracts the numeric start year
#' using the first four characters. Grades are coerced to character for processing, and then converted to numeric for plotting
#' by interpreting `"K"` as `0` and `"PK"`/`"PreK"` as `-1`. Any other non-numeric grades will result in `NA` unless handled separately.
#'
#' @import ggplot2
#' @importFrom stats aggregate setNames
#' @importFrom stats addmargins
#' @export
#' 
#' @examples
#' make_first_entered(dataset = math)
#' 

make_first_entered <- function(dataset, print_plot = FALSE) {
  # Validate and standardize column names
  colnames_lower <- tolower(names(dataset))
  
  # Required variables
  required_variables <- c("id", "grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  name_map <- setNames(names(dataset), colnames_lower)
  id_col <- name_map["id"]
  grade_col <- name_map["grade"]
  year_col <- name_map["year"]
  
  OUT <- list()
  OUT$Data <- as.data.frame(dataset)
  
  # Normalize grade values to numeric for sorting and manipulation
  OUT$Data[[grade_col]] <- normalize_grade(OUT$Data[[grade_col]])
  
  # Ensure YEAR is character and extract numeric start year
  OUT$Data[[year_col]] <- as.character(OUT$Data[[year_col]])
  OUT$Data$START_YEAR <- as.numeric(substr(OUT$Data[[year_col]], 1, 4))
  
  # Order by ID and START_YEAR
  OUT$Data <- OUT$Data[order(OUT$Data[[id_col]], OUT$Data$START_YEAR), ]
  
  # Temporary simplified dataset
  temporary <- OUT$Data[, c(id_col, grade_col, year_col)]
  names(temporary) <- c("ID", "GRADE", "YEAR")
  
  # Get the first record per student
  FIRST_INFO <- aggregate(cbind(GRADE, YEAR) ~ ID, data = temporary, FUN = function(x) x[1])
  names(FIRST_INFO)[names(FIRST_INFO) == "ID"] <- id_col
  FIRST_INFO$FIRST_ENTERED <- paste(FIRST_INFO$GRADE, FIRST_INFO$YEAR, sep = ".")
  
  # Merge back
  OUT$Data <- merge(dataset, FIRST_INFO[, c(id_col, "FIRST_ENTERED")], by = id_col, all.x = TRUE)
  
  # Make contingency table with totals
  grade_data <- normalize_grade(OUT$Data[[grade_col]])
  year_data <- OUT$Data[[year_col]]
  FIRST_ENTRY_TABLE <- table(grade_data, year_data)
  # Add totals to rows and columns
  FIRST_ENTRY_TABLE <- addmargins(FIRST_ENTRY_TABLE)
  OUT$Table <- FIRST_ENTRY_TABLE
  
  OUT$Caption <- "Count of First-Entry by Grade and Year (Totals Included)"
  
  # Create plotting dataframe
  FIRST_ENTRY_df <- as.data.frame(FIRST_ENTRY_TABLE)
  names(FIRST_ENTRY_df) <- c("Grade", "Year", "Count")
  FIRST_ENTRY_df$Grade <- as.numeric(as.character(FIRST_ENTRY_df$Grade))
  FIRST_ENTRY_df$Year <- as.numeric(substr(as.character(FIRST_ENTRY_df$Year), 1, 4))
  FIRST_ENTRY_df$Cohort <- FIRST_ENTRY_df$Year - FIRST_ENTRY_df$Grade
  
  # Line plot by cohort (over grades)
  PLOT1 <- ggplot(FIRST_ENTRY_df, aes(x = Grade, y = Count, group = Cohort, colour = as.factor(Cohort))) + 
    geom_line(linewidth = 1) + 
    geom_point(size = 2) + 
    labs(
      title = "First Entry Counts Year Over Grades", 
      x = "Grade", 
      y = "Number of First-Time Students", 
      colour = "Year in Kindergarten"
    ) + 
    theme_minimal() + 
    theme(legend.position = "bottom")
  OUT$Plot_by_Grade <- PLOT1
  
  # Line plot by year (over grades)
  PLOT2 <- ggplot(FIRST_ENTRY_df, aes(x = Year, y = Count, group = Grade, color = as.factor(Grade))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "First Entry Counts by Grade Over Years",
      x = "Academic Year",
      y = "Number of First-Time Students",
      color = "Grade Level"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  OUT$Plot_by_Year <- PLOT2
  
  if (print_plot) {
    print(PLOT1)
    print(PLOT2)
  }
  
  return(invisible(OUT))
}