#' Plot Student Mobility Distribution by Grade
#'
#' This function visualizes student mobility across grades between two academic years by computing the proportion of students who 
#' "Stay" (enrolled both years in the expected grade progression), "Left" (enrolled in the previous year but not the current year), 
#' or "Join" (enrolled in the current year but not the previous year). It supports flexible grade inputs (e.g., numeric, "K", "PK", "PreK").
#'
#' @param dataset A data frame containing at minimum the columns: `id` (student identifier), `grade` (grade level), and `year` 
#' (academic year, such as "2022-23" or just the numeric starting year "2022").
#'
#' @param current_year A numeric or character value indicating the current academic year of interest. If character, the function will 
#' extract the starting year (e.g., "2022-23" → 2022). The function compares this year to the previous year to calculate mobility.
#'
#' @param start_grade Optional. The lowest grade to include in the analysis. Accepts character (e.g., "K", "PK", "PreK") or numeric values. 
#' If `NULL`, defaults to 2.
#'
#' @param end_grade Optional. The highest grade to include in the analysis. Accepts character or numeric values. If `NULL`, defaults to 12.
#'
#' @param print_plot Logical. If `TRUE`, prints a bar chart of the mobility distribution to the plotting device. Default is `TRUE`.
#'
#' @return An (invisible) list with the following components:
#' \describe{
#'   \item{\code{Data}}{A data frame with one row per grade per mobility status, including columns:
#'     \code{MOBILITY} ("Leave", "Stay", or "Join"), \code{GRADE}, \code{COUNT}, \code{PROPORTION}, and \code{PERCENT}.}
#'   \item{\code{Plot}}{A ggplot2 object visualizing the mobility distribution across grade levels.}
#'   \item{\code{Table}}{A matrix of formatted percentages, cross-tabulated by mobility status (rows) and grade level (columns).}
#'   \item{\code{Caption}}{A caption string describing the year range used in the plot and table.}
#' }
#'
#' @details
#' This function assumes that students typically progress one grade level per year. For example, a student in Grade 4 in 2021-22 is 
#' expected to be in Grade 5 in 2022-23. The function identifies mobility based on this assumption and compares student `id`s between 
#' years and adjacent grades.
#'
#' The function also normalizes common grade label inputs using the internal \code{normalize_grade()} function:
#' \itemize{
#'   \item \code{"K"} → 0
#'   \item \code{"PK"}, \code{"PreK"}, \code{"prek"} → -1
#'   \item Numeric grades (e.g., 1–12) are kept as-is
#' }
#'
#' Missing data for `grade` or `year` is excluded prior to analysis.
#'
#' @seealso \code{\link{make_mobility}}
#'
#' @import ggplot2
#' @importFrom stats xtabs
#' @export
#' 
#' @examples
#' plot_distribution_mobility(dataset = math, current_year = 2022, start_grade = 4, end_grade = 6)
#' 
#' 

plot_distribution_mobility <- function(dataset, current_year, start_grade = NULL, end_grade = NULL, print_plot = TRUE) {
  # Input Validation
  colnames_lower <- tolower(names(dataset))
  required_variables <- c("id", "grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if (length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Mapping column names
  name_map <- setNames(names(dataset), colnames_lower)
  id_col <- name_map["id"]
  grade_col <- name_map["grade"]
  year_col <- name_map["year"]
  
  NEW <- as.data.frame(dataset)
  NEW <- NEW[!is.na(NEW[[grade_col]]) & !is.na(NEW[[year_col]]), ]
  NEW$NUMERIC_YEAR <- as.numeric(substr(NEW[[year_col]], 1, 4))
  if (is.character(current_year)) {
    current_year <- as.numeric(substr(current_year, 1, 4))
  }
  
  # Normalize grade
  NEW[[grade_col]] <- .normalize_grade(NEW[[grade_col]])
  
  if (is.null(start_grade)) start_grade <- 2
  if (is.null(end_grade)) end_grade <- 12
  
  from_grade <- .normalize_grade(start_grade)
  to_grade <- .normalize_grade(end_grade)
  GRADE_SEQUENCE <- seq(from = from_grade, to = to_grade, by = 1)
  
  OUT <- list()
  OUT$Data <- data.frame(
    MOBILITY = character(),
    GRADE = integer(),
    GRADE_PAIR_LABEL = character(),
    COUNT = integer(),
    PROPORTION = numeric(),
    stringsAsFactors = FALSE
  )
  
  STATUS <- c("Leave", "Stay", "Join")
  
  for (grade in GRADE_SEQUENCE) {
    NEW_STEP <- make_mobility(
      dataset = dataset,
      current_year = current_year,
      current_grade = grade,
      print_plot = FALSE
    )
    
    levels(NEW_STEP$Data$mobility_status) <- STATUS
    
    TABLE1 <- table(NEW_STEP$Data$mobility_status)
    TABLE2 <- prop.table(TABLE1)
    
    # Create grade pair label (e.g., "From 3 to 4")
    from_label <- grade - 1
    grade_pair_label <- paste0("From ", from_label, " to ", grade)
    
    TEMP <- data.frame(
      MOBILITY = factor(STATUS, levels = STATUS),
      GRADE = rep(grade, length(STATUS)),
      GRADE_PAIR_LABEL = rep(grade_pair_label, length(STATUS)),
      COUNT = as.numeric(TABLE1[STATUS]),
      PROPORTION = round(as.numeric(TABLE2[STATUS]), 3)
    )
    
    OUT$Data <- rbind(OUT$Data, TEMP)
  }
  
  OUT$Data$PERCENT <- OUT$Data$PROPORTION * 100
  OUT$Data$PERCENT[is.na(OUT$Data$PERCENT)] <- 0
  
  year_text <- paste0(current_year, "-", (current_year + 1))
  
  # Use grade pair labels on the x-axis
  PLOT1 <- ggplot(OUT$Data, aes(x = factor(GRADE_PAIR_LABEL, levels = unique(GRADE_PAIR_LABEL)), y = PERCENT, fill = MOBILITY)) + 
    geom_bar(stat = "identity") + 
    labs(
      title = paste("Mobility Status by Grade Transition for", year_text),
      y = "Percentage",
      x = "Grade Pair",
      fill = "Mobility Status"
    ) + 
    theme_minimal() +
    scale_fill_manual(values = c(
      "Leave" = "#A9A9A9",
      "Stay" = "#708090",
      "Join" = "#D3CFC4"
    ))
  
  OUT$Plot <- PLOT1
  
  # Create output table with labeled transitions
  table_df <- xtabs(PERCENT ~ MOBILITY + GRADE_PAIR_LABEL, data = OUT$Data)
  table_matrix <- as.matrix(table_df)
  table_matrix <- apply(table_matrix, c(1, 2), function(x) sprintf("%.1f%%", x))
  colnames(table_matrix) <- paste0("From Grade ", GRADE_SEQUENCE - 1, " to Grade ", GRADE_SEQUENCE)
  OUT$Table <- table_matrix
  
  # Make caption
  OUT$Caption <- paste("Mobility Distribution by Grade Transition for", year_text)
  
  if (print_plot) print(PLOT1)
  
  return(invisible(OUT))
}

