#' Analyze Grade-Level Entry Patterns Across Academic Years
#'
#' @description
#' Identifies and summarizes how students enter or skip a specified grade across consecutive academic years.
#' This function classifies students into entry categories such as Repeat, Stay, Skip-In, Skip-Over, and Join,
#' and returns summary tables, student-level data, and longitudinal visualizations.
#'
#' This function is useful for understanding how grade-level composition changes over time and how these changes
#' may relate to cohort performance on assessments.
#'
#' @param dataset A data frame containing at least the variables `ID`, `GRADE`, and `YEAR`.
#' @param grade The target grade of interest. Can be numeric or character (e.g., `"5"` or `"K"`).
#'
#' @return A named list (invisibly) with the following elements:
#'
#' \describe{
#'   \item{Tables}{A list of summary tables for each entry type (`Repeat`, `Stay`, `Skip_In`, `Skip_Over`, `Join`), 
#'                each containing `Academic_Year`, `Count`, and `Percent` columns. Also includes a combined table `All`.}
#'   \item{Data}{A list of student-level data frames for each entry type, filtered for relevant rows.}
#'   \item{Meta}{A named vector `Total_Target_Grade` with the total number of students in the target grade by year.}
#'   \item{Plots}{A `Line` plot (ggplot2 object) showing how the percentage of each entry type changes over time.}
#' }
#'
#' @details
#' Entry types are classified as follows:
#'
#' - **Repeat**: Student was in the target grade both this year and the prior year.
#' - **Stay**: Student was in grade `target - 1` last year and advanced normally to the target grade.
#' - **Skip_In**: Student jumped two or more grades into the target grade from a lower grade last year.
#' - **Skip_Over**: Student was in a lower grade last year and a higher grade this year, skipping the target grade.
#' - **Join**: Student appears in the target grade this year but was not observed at all in the prior year.
#'
#' @import ggplot2
#' @importFrom stats setNames
#' @export

analyze_grade_changes <- function(dataset, grade) {
  #--- Validate inputs ---
  if (!all(c("ID", "GRADE", "YEAR") %in% names(dataset))) {
    stop("Dataset must include columns: ID, GRADE, and YEAR.")
  }
  
  #--- Normalize grade and year ---
  df <- dataset
  df$GRADE_NUM <- .normalize_grade(df$GRADE)
  df$YEAR_NUM <- parse_year(df$YEAR)
  target_num <- .normalize_grade(grade)
  
  years <- sort(unique(df$YEAR_NUM))
  year_labels <- to_academic_year(years)
  OUT <- list(Tables = list(), Data = list(), Meta = list(), Plots = list())
  
  #--- Initialize containers ---
  categories <- c("Repeat", "Stay", "Skip_In", "Skip_Over", "Join")
  counts <- lapply(categories, function(x) integer(length(years)))
  names(counts) <- categories
  rows <- lapply(categories, function(x) list())
  names(rows) <- categories
  total_counts <- integer(length(years))
  
  #--- Loop over year pairs ---
  for (i in 2:length(years)) {
    curr_year <- years[i]
    prev_year <- years[i - 1]
    y_label <- to_academic_year(curr_year)
    
    curr <- df[df$YEAR_NUM == curr_year, ]
    prev <- df[df$YEAR_NUM == prev_year, ]
    
    # Subsets
    curr_g      <- curr[curr$GRADE_NUM == target_num, ]
    prev_g      <- prev[prev$GRADE_NUM == target_num, ]
    prev_stay   <- prev[prev$GRADE_NUM == (target_num - 1), ]
    prev_skipin <- prev[prev$GRADE_NUM < (target_num - 1), ]
    prev_skipover <- prev[prev$GRADE_NUM < target_num, ]
    curr_skipover <- curr[curr$GRADE_NUM > target_num, ]
    
    # Total target grade students (for denominator)
    total_counts[i] <- nrow(curr_g)
    
    # Match IDs
    ids_repeat     <- intersect(curr_g$ID, prev_g$ID)
    ids_stay       <- intersect(curr_g$ID, prev_stay$ID)
    ids_skipin     <- intersect(curr_g$ID, prev_skipin$ID)
    ids_skipover   <- intersect(curr_skipover$ID, prev_skipover$ID)
    ids_join       <- setdiff(curr_g$ID, prev$ID)
    
    # Store counts and data
    counts$Repeat[i]    <- length(ids_repeat)
    counts$Stay[i]      <- length(setdiff(ids_stay, ids_repeat))       # exclude repeaters
    counts$Skip_In[i]   <- length(setdiff(ids_skipin, ids_stay))       # exclude those already counted
    counts$Skip_Over[i] <- length(ids_skipover)
    counts$Join[i]      <- length(ids_join)
    
    rows$Repeat[[y_label]]    <- curr_g[curr_g$ID %in% ids_repeat, ]
    rows$Stay[[y_label]]      <- curr_g[curr_g$ID %in% setdiff(ids_stay, ids_repeat), ]
    rows$Skip_In[[y_label]]   <- curr_g[curr_g$ID %in% setdiff(ids_skipin, ids_stay), ]
    rows$Skip_Over[[y_label]] <- curr_skipover[curr_skipover$ID %in% ids_skipover, ]
    rows$Join[[y_label]]      <- curr_g[curr_g$ID %in% ids_join, ]
  }
  
  #--- Convert to tables with Percent ---
  academic_years <- year_labels[-1]
  total_counts_trim <- total_counts[-1]
  OUT$Meta$Total_Target_Grade <- setNames(total_counts_trim, academic_years)
  
  for (cat in categories) {
    this_counts <- counts[[cat]][-1]
    this_perc <- ifelse(total_counts_trim > 0, round(100 * this_counts / total_counts_trim, 1), NA)
    OUT$Tables[[cat]] <- data.frame(
      Academic_Year = academic_years,
      Count = this_counts,
      Percent = this_perc
    )
    OUT$Data[[cat]] <- do.call(rbind, rows[[cat]])
  }
  
  #--- Combined summary table ---
  tbl_all <- data.frame(
    Academic_Year = academic_years,
    Total = total_counts_trim,
    Repeat = counts$Repeat[-1],
    Stay = counts$Stay[-1],
    Skip_In = counts$Skip_In[-1],
    Skip_Over = counts$Skip_Over[-1],
    Join = counts$Join[-1]
  )
  OUT$Tables$All <- tbl_all
  
  #--- Line plot of % over time ---
  longplot_df <- do.call(rbind, lapply(categories, function(cat) {
    data.frame(
      Academic_Year = academic_years,
      Entry_Type = cat,
      Percent = OUT$Tables[[cat]]$Percent
    )
  }))
  
  OUT$Plots$Line <- ggplot(longplot_df, aes(x = Academic_Year, y = Percent, group = Entry_Type, color = Entry_Type)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      title = paste("Grade", grade, "- Entry Type Composition Over Time"),
      x = "Academic Year",
      y = "Percent of Students"
    ) +
    theme_minimal()
  
  invisible(OUT)
}