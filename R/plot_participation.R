#' Plot Assessment Participation by Content Area and Grade
#'
#' @description
#' Generates visualizations and summary tables showing student participation in assessments
#' over time. Participation is grouped by academic year and broken out by content area and grade level.
#' Useful for understanding test coverage patterns, grade-level shifts, and participation gaps.
#'
#' This function outputs both faceted and individual plots by content area, as well as
#' a set of participation tables by grade and academic year.
#'
#' @param dataset A data frame containing the following required columns:
#'   - `ID`: Unique student identifier
#'   - `YEAR`: The testing year (can be character or numeric)
#'   - `CONTENT_AREA`: The subject or test name (e.g., "MATHEMATICS", "ELA")
#'   - `GRADE`: The grade level of the student at the time of assessment
#'
#' @return A named list with the following elements:
#'
#' \describe{
#'   \item{Overall}{A faceted `ggplot` line chart showing total participation over time by content area.}
#'   \item{Overall_Individual}{A named list of un-faceted `ggplot` plots, one per content area.}
#'   \item{Overall_Caption}{A character string summarizing the purpose of the overall plot.}
#'
#'   \item{Detailed}{A faceted `ggplot` line chart showing grade-level participation over time.}
#'   \item{Detailed_Individual}{A named list of un-faceted `ggplot` plots showing grade trends for each content area.}
#'   \item{Detailed_Caption}{A character string summarizing the purpose of the detailed plot.}
#'
#'   \item{Tables}{A named list of wide-format grade × year tables, one per content area. Column names are formatted as academic years.}
#' }
#'
#' @details
#' This function assumes one row per student per assessment instance. Duplicate rows may inflate participation counts.
#' Academic year labels are derived using internal helper functions `.normalize_grade()`, `parse_year()`, and `to_academic_year()`.
#'
#' Plots are formatted using `ggplot2` with minimal styling for easy embedding into automated reports.
#'
#' @import ggplot2
#' @importFrom stats aggregate reshape
#' 
#' @export
#' 
#' @examples
#' plot_participation(dataset = math)
#' 

plot_participation <- function(dataset) {
  #--- Check for required columns ---
  required_vars <- c("ID", "YEAR", "CONTENT_AREA", "GRADE")
  missing_vars <- setdiff(required_vars, names(dataset))
  if (length(missing_vars) > 0) {
    stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
  }
  
  #--- Clean and prepare dataset ---
  df <- dataset[!is.na(dataset$ID) & !is.na(dataset$YEAR) &
                  !is.na(dataset$CONTENT_AREA) & !is.na(dataset$GRADE), ]
  
  df$GRADE <- as.character(df$GRADE)
  df$YEAR <- as.character(df$YEAR)
  df$YEAR_LABEL <- df$YEAR
  df$GRADE_NUM <- .normalize_grade(df$GRADE)
  df$YEAR_NUM <- parse_year(df$YEAR)
  
  #--- Create academic year labels ---
  year_labels <- unique(df[, c("YEAR_NUM", "YEAR_LABEL")])
  year_labels$YEAR_ACAD <- to_academic_year(year_labels$YEAR_NUM)
  
  #--- Set up output list ---
  OUT <- list()
  
  #--- Overall Plot (Facetted) ---
  counts_overall <- aggregate(ID ~ YEAR_NUM + YEAR_LABEL + CONTENT_AREA, data = df, FUN = function(x) length(unique(x)))
  names(counts_overall)[names(counts_overall) == "ID"] <- "n_students"
  
  overall_plot <- ggplot(counts_overall, aes(x = YEAR_NUM, y = n_students)) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(color = "steelblue") +
    facet_wrap(~ CONTENT_AREA, scales = "free_y") +
    scale_x_continuous(
      breaks = year_labels$YEAR_NUM,
      labels = year_labels$YEAR_ACAD
    ) +
    labs(
      title = "Assessment Participation Over Time",
      x = "Academic Year",
      y = "Number of Students"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  OUT$Overall <- overall_plot
  OUT$Overall_Caption <- "This chart shows the total number of students taking each assessment over time. Each panel represents a different content area, with participation counts plotted by academic year."
  
  #--- Overall Plot (Unfacetted) ---
  OUT$Overall_Individual <- list()
  content_areas <- unique(counts_overall$CONTENT_AREA)
  
  for (area in content_areas) {
    df_area <- counts_overall[counts_overall$CONTENT_AREA == area, ]
    p <- ggplot(df_area, aes(x = YEAR_NUM, y = n_students)) +
      geom_line(linewidth = 1, color = "steelblue") +
      geom_point(color = "steelblue") +
      scale_x_continuous(
        breaks = year_labels$YEAR_NUM,
        labels = year_labels$YEAR_ACAD
      ) +
      labs(
        title = paste("Participation in", area, "Over Time"),
        x = "Academic Year",
        y = "Number of Students"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    OUT$Overall_Individual[[area]] <- p
  }
  
  #--- Detailed Plot (Facetted) ---
  counts_detailed <- aggregate(ID ~ YEAR_NUM + YEAR_LABEL + CONTENT_AREA + GRADE, data = df, FUN = function(x) length(unique(x)))
  names(counts_detailed)[names(counts_detailed) == "ID"] <- "n_students"
  
  counts_detailed$GRADE <- as.character(counts_detailed$GRADE)
  counts_detailed$GRADE_NUM <- .normalize_grade(counts_detailed$GRADE)
  counts_detailed <- counts_detailed[!is.na(counts_detailed$GRADE_NUM), ]
  
  grade_levels <- unique(counts_detailed[order(counts_detailed$GRADE_NUM, counts_detailed$GRADE), "GRADE"])
  counts_detailed$GRADE <- factor(counts_detailed$GRADE, levels = grade_levels)
  
  detailed_plot <- ggplot(counts_detailed, aes(x = YEAR_NUM, y = n_students, color = GRADE, group = GRADE)) +
    geom_line(linewidth = 1) +
    geom_point() +
    facet_wrap(~ CONTENT_AREA, scales = "free_y") +
    scale_x_continuous(
      breaks = year_labels$YEAR_NUM,
      labels = year_labels$YEAR_ACAD
    ) +
    labs(
      title = "Assessment Participation by Grade Over Time",
      x = "Academic Year",
      y = "Number of Students",
      color = "Grade"
    ) +
    theme_minimal() +
    theme(
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  OUT$Detailed <- detailed_plot
  OUT$Detailed_Caption <- "This chart breaks down participation by grade level for each content area. Lines represent different grades, allowing for trends in grade-level test-taking to be compared across years."
  
  #--- Detailed Plot (Unfacetted) ---
  OUT$Detailed_Individual <- list()
  
  for (area in content_areas) {
    df_area <- counts_detailed[counts_detailed$CONTENT_AREA == area, ]
    p <- ggplot(df_area, aes(x = YEAR_NUM, y = n_students, color = GRADE, group = GRADE)) +
      geom_line(linewidth = 1) +
      geom_point() +
      scale_x_continuous(
        breaks = year_labels$YEAR_NUM,
        labels = year_labels$YEAR_ACAD
      ) +
      labs(
        title = paste("Participation by Grade in", area),
        x = "Academic Year",
        y = "Number of Students",
        color = "Grade"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    OUT$Detailed_Individual[[area]] <- p
  }
  
  #--- Tables: one grade × year table per CONTENT_AREA ---
  OUT$Tables <- list()
  
  for (area in content_areas) {
    df_area <- df[df$CONTENT_AREA == area, ]
    counts_table <- aggregate(ID ~ GRADE + YEAR_LABEL, data = df_area, FUN = function(x) length(unique(x)))
    names(counts_table)[names(counts_table) == "ID"] <- "n_students"
    
    table_wide <- reshape(
      counts_table,
      timevar = "YEAR_LABEL",
      idvar = "GRADE",
      direction = "wide"
    )
    
    table_wide$GRADE_NUM <- .normalize_grade(table_wide$GRADE)
    table_wide <- table_wide[order(table_wide$GRADE_NUM, table_wide$GRADE), ]
    table_wide$GRADE_NUM <- NULL
    
    colnames(table_wide) <- sub("^n_students\\.", "", colnames(table_wide))
    year_cols <- setdiff(names(table_wide), "GRADE")
    formatted_years <- setNames(year_labels$YEAR_ACAD, year_labels$YEAR_LABEL)[year_cols]
    colnames(table_wide)[match(year_cols, names(table_wide))] <- formatted_years
    
    total_row <- c("Total", colSums(table_wide[formatted_years], na.rm = TRUE))
    table_wide <- rbind(table_wide, total_row)
    rownames(table_wide) <- NULL
    
    OUT$Tables[[area]] <- table_wide
  }
  
  return(invisible(OUT))
}

