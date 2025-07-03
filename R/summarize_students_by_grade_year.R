#' @title Summarize Enrollment by Grade and Year
#'
#' @description
#' This function generates tables, summary statistics, and visualizations of student enrollment patterns
#' by grade level and academic year. It supports flexible column names and provides outputs in both
#' long and wide formats, along with heatmaps, line plots, and stacked bar charts for reporting purposes.
#'
#' @param dataset A data frame containing at least student ID, grade, and year columns.
#' 
#' @param id_variables A named list specifying the column names to use for:
#'
#' - \code{student_id}: unique student identifier (default "ID")
#' - \code{grade}: student grade level (default "GRADE")
#' - \code{year}: academic year (default "YEAR")
#'
#' @return A named list with the following components:
#'
#' - **Standard_Data**: standardized dataset with renamed columns and numeric grade
#' - **Summary_Long**: long-format table of student counts by grade and year
#' - **Summary_Wide**: wide-format table with academic years as columns
#' - **Grade_Statistics**: summary statistics (mean, SD, median, min, max) by grade
#' - **Total_Enrollment**: total student enrollment per year
#' - **Heatmap**: ggplot heatmap of student counts
#' - **Enrollment_LinePlot**: ggplot line plot of enrollment trends by grade
#' - **Enrollment_BarPlot**: ggplot stacked bar chart of grade enrollment per year
#'
#' @details
#' Grade levels are normalized to numeric values using the internal helper
#' \code{.normalize_grade()} for correct ordering. Year values are parsed using \code{.parse_year()}
#' and converted to academic year labels (e.g., "2022-2023") for use in tables and plots.
#'
#' This function is designed for use in automated reporting workflows (e.g., Quarto)
#' and provides modular outputs for integration into templated documents.
#'
#' @import ggplot2
#' @importFrom stats aggregate reshape
#' 
#' @export
#'
#' @examples
#' summarize_students_by_grade_year(dataset = math)
#' 

summarize_students_by_grade_year <- function(
    dataset,
    id_variables = list(student_id = "ID", grade = "GRADE", year = "YEAR")
) {
  # Check that input is a data frame
  if (!is.data.frame(dataset)) stop("'dataset' must be a data frame.")
  
  # Validate that id_variables has all required keys
  expected_keys <- c("student_id", "grade", "year")
  missing_keys <- setdiff(expected_keys, names(id_variables))
  if (length(missing_keys) > 0) {
    stop("id_variables is missing required keys: ", paste(missing_keys, collapse = ", "))
  }
  
  # Extract variable names from id_variables
  student_id_variable <- id_variables$student_id
  grade_variable      <- id_variables$grade
  year_variable       <- id_variables$year
  
  # Check that all specified columns exist in the dataset
  required_columns <- unlist(id_variables)
  missing_columns <- setdiff(required_columns, names(dataset))
  if (length(missing_columns) > 0) {
    stop("Dataset is missing required variables: ", paste(missing_columns, collapse = ", "))
  }
  
  # Initialize output list
  OUT <- list()
  
  # Standardize column names for internal processing
  data_standard <- dataset
  names(data_standard)[names(data_standard) == student_id_variable] <- "STUDENT_ID"
  names(data_standard)[names(data_standard) == grade_variable]      <- "GRADE"
  names(data_standard)[names(data_standard) == year_variable]       <- "YEAR"
  
  # Normalize YEAR to numeric using internal helper
  data_standard$YEAR <- .parse_year(data_standard$YEAR)
  
  # Normalize GRADE to numeric using internal helper for sorting
  data_standard$GRADE_NUMERIC <- .normalize_grade(data_standard$GRADE)
  
  # Save standardized dataset to output
  OUT$Standard_Data <- data_standard
  
  # Remove duplicate student-grade-year records
  unique_records <- data_standard[!duplicated(data_standard[c("STUDENT_ID", "GRADE_NUMERIC", "YEAR")]), ]
  
  # Count students by numeric grade and year (long format)
  long_table <- aggregate(STUDENT_ID ~ GRADE_NUMERIC + YEAR, data = unique_records, FUN = length)
  names(long_table)[names(long_table) == "STUDENT_ID"] <- "Count"
  
  # Merge original grade labels back in
  grade_labels <- unique(data_standard[c("GRADE_NUMERIC", "GRADE")])
  long_table <- merge(long_table, grade_labels, by = "GRADE_NUMERIC", all.x = TRUE)
  
  # Order long table by numeric grade and year
  long_table <- long_table[order(long_table$GRADE_NUMERIC, long_table$YEAR), ]
  
  # Add academic year label (e.g., "2022-2023")
  long_table$YEAR_LABEL <- .to_academic_year(long_table$YEAR)
  
  # Save long format table to output
  OUT$Summary_Long <- long_table
  
  # Reshape to wide format with numeric grade as row ID and year as columns
  wide_table <- reshape(
    long_table[c("GRADE_NUMERIC", "YEAR", "Count")],
    timevar = "YEAR",
    idvar = "GRADE_NUMERIC",
    direction = "wide"
  )
  
  # Replace any NAs with 0
  wide_table[is.na(wide_table)] <- 0
  
  # Rename "Count.2022" to "2022-2023"
  names(wide_table) <- sub("^Count\\.", "", names(wide_table))
  year_cols <- setdiff(names(wide_table), "GRADE_NUMERIC")
  new_names <- .to_academic_year(as.integer(year_cols))
  names(wide_table)[match(year_cols, names(wide_table))] <- new_names
  
  # Add original grade labels back and reorder
  wide_table <- merge(grade_labels, wide_table, by = "GRADE_NUMERIC", all.x = TRUE)
  wide_table <- wide_table[order(wide_table$GRADE_NUMERIC), ]
  
  # Clean final wide table for display
  rownames(wide_table) <- wide_table$GRADE
  wide_table$GRADE <- NULL
  wide_table$GRADE_NUMERIC <- NULL
  
  # Save to output
  OUT$Summary_Wide <- wide_table
  
  # Compute summary statistics
  grade_list <- split(long_table$Count, long_table$GRADE_NUMERIC)
  
  grade_stats_df <- data.frame(
    GRADE_NUMERIC = as.numeric(names(grade_list)),
    MEAN = round(sapply(grade_list, mean), 1),
    SD = round(sapply(grade_list, function(x) if(length(x) > 1) sd(x) else NA_real_), 1),
    MEDIAN = round(sapply(grade_list, median), 1),
    MIN = sapply(grade_list, min),
    MAX = sapply(grade_list, max)
  )
  
  # Save grade statistics to output
  OUT$Grade_Statistics <- grade_stats_df
  
  # Calculate total enrollment per year for trend detection
  total_enrollment <- aggregate(Count ~ YEAR + YEAR_LABEL, data = long_table, FUN = sum)
  
  # Save total enrollment summary to output
  OUT$Total_Enrollment <- total_enrollment
  
  # Create heatmap using academic year label and original grade order
  heatmap_plot <- ggplot(long_table, aes(
    x = factor(YEAR_LABEL),
    y = factor(GRADE, levels = unique(GRADE[order(GRADE_NUMERIC)])),
    fill = Count
  )) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), color = "black", size = 3) +
    scale_fill_gradient(low = "#e0ecf4", high = "#084594") +
    labs(
      title = "Student Counts by Grade and Year",
      x = "School Year",
      y = "Grade Level",
      fill = "Count"
    ) +
    theme_minimal()
  
  # Add plot to output
  OUT$Heatmap <- heatmap_plot
  
  # Prepare total enrollment by grade and year for plots
  total_enrollment_grade <- long_table[, c("GRADE", "YEAR_LABEL", "Count")]
  
  # Order YEAR_LABEL factor by chronological year
  year_levels <- unique(long_table$YEAR_LABEL[order(long_table$YEAR)])
  total_enrollment_grade$YEAR_LABEL <- factor(total_enrollment_grade$YEAR_LABEL, levels = year_levels)
  
  # Create ordered levels for GRADE by numeric grade
  grade_order_df <- unique(long_table[c("GRADE", "GRADE_NUMERIC")])
  grade_order_df <- grade_order_df[order(grade_order_df$GRADE_NUMERIC), ]
  grade_levels <- grade_order_df$GRADE
  total_enrollment_grade$GRADE <- factor(total_enrollment_grade$GRADE, levels = grade_levels)
  
  # Line Plot for enrollment trends by grade
  line_plot <- ggplot(total_enrollment_grade, aes(x = YEAR_LABEL, y = Count, group = GRADE, color = GRADE)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Enrollment Trends by Grade Over Years",
      x = "Academic Year",
      y = "Number of Students",
      color = "Grade"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  OUT$Enrollment_LinePlot <- line_plot
  
  # Stacked Bar Plot for enrollment by grade
  bar_plot <- ggplot(total_enrollment_grade, aes(x = YEAR_LABEL, y = Count, fill = GRADE)) +
    geom_bar(stat = "identity", position = "stack", color = "black", size = 0.3) +  # thin black border
    labs(
      title = "Stacked Enrollment by Grade Over Years",
      x = "Academic Year",
      y = "Number of Students",
      fill = "Grade"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  OUT$Enrollment_BarPlot <- bar_plot
  
  # Return everything
  return(invisible(OUT))
}
