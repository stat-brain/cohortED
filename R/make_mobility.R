#' Calculate Student Mobility Between Grades and Years
#'
#' This function analyzes student mobility status by comparing student IDs
#' between two consecutive academic years and grades. It categorizes students as
#' "Leave" (present in previous year/grade only), "Join" (present in current year/grade only),
#' or "Stay" (present in both). It returns a summary table, a merged dataset with mobility
#' status, and a bar plot visualizing the distribution.
#'
#' @title Make Mobility
#' 
#' @param dataset A data frame containing student-level data. Must include columns for
#'   student ID, grade, and academic year. Column names are case-insensitive.
#'   The grade column may contain numeric values, or special codes:
#'   \itemize{
#'     \item `"K"` for Kindergarten (treated as 0),
#'     \item `"PK"` or `"PreK"` for Pre-Kindergarten (treated as -1).
#'   }
#' @param current_year The academic year of the current grade (e.g., `"2020-2021"` or numeric year `2020`).
#'   Can be a string or numeric. The function extracts the starting calendar year if a string.
#' @param current_grade Numeric or convertible to numeric, or a string representing grade level.
#'   Supports `"K"`, `"PK"`, and `"PreK"` as described above.
#' @param print_plot Logical (default: \code{FALSE}). If \code{TRUE}, prints the mobility distribution bar plot.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{Data}{A data frame merging student data from previous and current year/grade with mobility status.}
#'   \item{Table}{A one-row matrix with percentages of students who "Leave", "Stay", or "Join".}
#'   \item{Caption}{A character string describing the grade and year comparison used.}
#'   \item{Barplot}{A ggplot2 bar plot object visualizing mobility distribution percentages.}
#' }
#'
#' @details
#' The function:
#' \itemize{
#'   \item Validates required columns ("id", "grade", "year") exist (case-insensitive).
#'   \item Converts special grade codes `"K"`, `"PK"`, and `"PreK"` to numeric equivalents (`0` and `-1` respectively) for computation.
#'   \item Extracts the calendar year from the academic year (first 4 characters).
#'   \item Computes mobility status by comparing student IDs from the previous grade/year
#'     to the current grade/year.
#'   \item Always returns percentages for all three mobility categories, even if some have zero counts.
#'   \item Produces a bar plot showing the distribution with custom colors for each status.
#' }
#' 
#' @importFrom stats aggregate setNames
#' @import ggplot2
#' @export
#' 
#' @examples
#' make_mobility(dataset = math, current_year = "2020_2021", current_grade = 4)
#' make_mobility(dataset = math, current_year = 2022, current_grade = 7)
#' 

make_mobility <- function(dataset, current_year, current_grade, print_plot = FALSE) {
  # Standardize column names to lowercase
  names(dataset) <- tolower(names(dataset))
  colnames_lower <- names(dataset)
  
  # Required variable check
  required_variables <- c("id", "grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if (length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Work directly with lowercased names
  id_col <- "id"
  grade_col <- "grade"
  year_col <- "year"
  
  NEW <- as.data.frame(dataset)
  NEW <- NEW[!is.na(NEW[[grade_col]]) & !is.na(NEW[[year_col]]), ]
  
  NEW$numeric_year <- as.numeric(substr(NEW[[year_col]], 1, 4))
  if (is.character(current_year)) {
    current_year <- as.numeric(substr(current_year, 1, 4))
  }
  
  # Normalize grades
  NEW[[grade_col]] <- normalize_grade(NEW[[grade_col]])
  current_grade <- normalize_grade(current_grade)
  
  previous_year <- current_year - 1
  previous_grade <- current_grade - 1
  
  START <- NEW[NEW$numeric_year == previous_year & NEW[[grade_col]] == previous_grade, ]
  END <- NEW[NEW$numeric_year == current_year & NEW[[grade_col]] == current_grade, ]
  
  START$numeric_year <- NULL
  END$numeric_year <- NULL
  
  START_ID <- unique(START[[id_col]])
  END_ID <- unique(END[[id_col]])
  ALL_ID <- union(START_ID, END_ID)
  
  MOBILITY <- ifelse(
    ALL_ID %in% START_ID & ALL_ID %in% END_ID, "Stay",
    ifelse(ALL_ID %in% START_ID, "Leave", "Join")
  )
  MOBILITY <- factor(MOBILITY, levels = c("Leave", "Stay", "Join"))
  
  MOBILITY_df <- data.frame(id = ALL_ID, mobility_status = MOBILITY)
  
  # Merge START and END, then join with mobility labels
  OUT <- list()
  OUT$Data <- merge(x = START, y = END, by = id_col, all = TRUE)
  OUT$Data <- merge(x = OUT$Data, y = MOBILITY_df, by = id_col, all = TRUE)
  
  # Proportion summary table
  TABLE1_raw <- prop.table(table(OUT$Data$mobility_status)) * 100
  TABLE1 <- setNames(rep(0, 3), c("Leave", "Stay", "Join"))
  TABLE1[names(TABLE1_raw)] <- round(TABLE1_raw[names(TABLE1_raw)], 1)
  TABLE2 <- sapply(TABLE1, function(x) sprintf("%.1f%%", x))
  OUT$Table <- t(TABLE2)
  
  previous_text <- paste0("Grade ", previous_grade, " in ", previous_year, "-", current_year)
  current_text <- paste0("Grade ", current_grade, " in ", current_year, "-", (current_year + 1))
  OUT$Caption <- paste("From ", previous_text, " to ", current_text)
  
  PLOT_df <- data.frame(
    MOBILITY = names(TABLE1),
    PERCENT = as.numeric(TABLE1)
  )
  
  PLOT1 <- ggplot(PLOT_df, aes(x = MOBILITY, y = PERCENT, fill = MOBILITY)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(
      "Leave" = "darkgray",
      "Stay" = "steelblue",
      "Join" = "lightgray"
    )) +
    labs(title = "Mobility Distribution", subtitle = OUT$Caption, y = "Percent", x = NULL) +
    theme_minimal()
  
  OUT$Barplot <- PLOT1
  
  if (print_plot) print(PLOT1)
  
  return(invisible(OUT))
}

