#' Compare Achievement Levels by Student Mobility Status Across Two Years
#'
#' This function compares the distribution of achievement levels among students grouped by
#' mobility status (e.g., Stay, Join, Leave) across two consecutive school years.
#' It returns tables, plots, and summary data useful for analyzing cohort stability
#' and performance trends. The "Stay" group represents a true cohort of students observed
#' in both years, while the "Join" and "Leave" groups represent those entering or exiting.
#'
#' @param dataset A data frame containing student-level data, including identifiers, grade, year, mobility status, and achievement levels.
#' @param current_year A character string giving the current school year (e.g., \code{"2023-2024"}).
#' @param current_grade An integer giving the current grade level (e.g., \code{5}).
#' @param achievement_levels A character vector defining the ordered achievement levels to display (e.g., \code{c("Unsatisfactory", "Partially Proficient", "Proficient", "Advanced")}).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{Previous_Table}}{A matrix of percent proficient by mobility group from the previous school year.}
#'   \item{\code{Current_Table}}{A matrix of percent proficient by mobility group from the current school year.}
#'   \item{\code{Data}}{A combined long-format data frame of achievement level percentages by mobility group across both years.}
#'   \item{\code{Group_Sizes}}{A list of data frames showing the number of students per mobility group for each year.}
#'   \item{\code{Most_Common_Level}}{A list of data frames showing the most common achievement level for each mobility group in each year.}
#'   \item{\code{Change_Summary}}{A data frame showing the percent change in each achievement level by mobility group across years.}
#'   \item{\code{Plot}}{A side-by-side bar plot comparing achievement distributions by mobility status for each year.}
#'   \item{\code{Caption}}{A character string describing the comparison.}
#'   \item{\code{Note}}{A short note clarifying that "Stay" represents a true cohort; "Join" and "Leave" are not tracked longitudinally.}
#' }
#'
#' @details
#' Internally calls \code{\link{plot_alluvial_mobility}} to construct matched student data across years.
#' Students with missing or "No Score" achievement data are excluded from analysis.
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs scale_fill_manual theme_minimal
#' @importFrom cowplot plot_grid
#' @importFrom stats xtabs
#' @export
#' 
#' @examples
#' compare_achievement_mobility(
#'   dataset = math,
#'   current_year = "2023-2024",
#'   current_grade = 5,
#'   achievement_levels = c("Advanced", "Proficient", "Partially Proficient", "Unsatisfactory")
#' )
#'  


compare_achievement_mobility <- function(dataset, current_year, current_grade, achievement_levels) {
  # Get student-level data with mobility and achievement levels
  NEW_DATA <- plot_alluvial_mobility(
    dataset = dataset,
    current_year = current_year,
    current_grade = current_grade,
    print_plot = FALSE
  )$Data
  
  # Extract numeric year and create label strings
  current_year_num <- as.numeric(substr(current_year, 1, 4))
  last_year_text <- paste0(current_year_num - 1, "-", current_year_num)
  current_year_text <- paste0(current_year_num, "-", current_year_num + 1)
  
  # Filter valid scores (remove "No Score" and NA)
  DATA_prev <- NEW_DATA[!is.na(NEW_DATA$achievement_level.x) & NEW_DATA$achievement_level.x != "No Score", ]
  DATA_curr <- NEW_DATA[!is.na(NEW_DATA$achievement_level.y) & NEW_DATA$achievement_level.y != "No Score", ]
  
  # Create percent tables for each year
  table_prev <- table(DATA_prev$mobility_status, DATA_prev$achievement_level.x)
  df_prev <- data.frame(round(prop.table(table_prev, margin = 1), 3) * 100)
  df_prev <- df_prev[!is.nan(df_prev$Freq), ]
  names(df_prev) <- c("Mobility_Status", "Achievement_Level", "Percent")
  df_prev$Achievement_Level <- factor(df_prev$Achievement_Level, levels = achievement_levels)
  df_prev$Grade <- current_grade - 1
  df_prev$Year <- paste0(current_year_num - 1, "_", current_year_num)
  
  table_curr <- table(DATA_curr$mobility_status, DATA_curr$achievement_level.y)
  df_curr <- data.frame(round(prop.table(table_curr, margin = 1), 3) * 100)
  df_curr <- df_curr[!is.nan(df_curr$Freq), ]
  names(df_curr) <- c("Mobility_Status", "Achievement_Level", "Percent")
  df_curr$Achievement_Level <- factor(df_curr$Achievement_Level, levels = achievement_levels)
  df_curr$Grade <- current_grade
  df_curr$Year <- paste0(current_year_num, "_", current_year_num + 1)
  
  # Combine into a single dataset
  combined_df <- rbind(df_prev, df_curr)
  
  # Bar plot for previous year
  plot_prev <- ggplot(df_prev, aes(x = Mobility_Status, y = Percent, fill = Achievement_Level)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Grade", current_grade - 1, "in", last_year_text),
      y = "Percent",
      x = "Mobility Status",
      fill = "Achievement Level"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c(
      "Advanced" = "#2E8B57",
      "Proficient" = "#66CDAA",
      "Partially Proficient" = "#E57373",
      "Unsatisfactory" = "#B22222"
    ))
  
  # Bar plot for current year
  plot_curr <- ggplot(df_curr, aes(x = Mobility_Status, y = Percent, fill = Achievement_Level)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Grade", current_grade, "in", current_year_text),
      y = "Percent",
      x = "Mobility Status",
      fill = "Achievement Level"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c(
      "Advanced" = "#2E8B57",
      "Proficient" = "#66CDAA",
      "Partially Proficient" = "#E57373",
      "Unsatisfactory" = "#B22222"
    ))
  
  # Summary tables (cross-tab)
  previous_table <- xtabs(Percent ~ Mobility_Status + Achievement_Level, data = df_prev)
  current_table  <- xtabs(Percent ~ Mobility_Status + Achievement_Level, data = df_curr)
  
  # Group sizes by mobility
  group_sizes <- list(
    Previous = as.data.frame(table(DATA_prev$mobility_status)),
    Current = as.data.frame(table(DATA_curr$mobility_status))
  )
  names(group_sizes$Previous) <- c("Mobility_Status", "Count")
  names(group_sizes$Current) <- c("Mobility_Status", "Count")
  
  # Most common achievement level
  most_common <- list(
    Previous = get_most_common(df_prev),
    Current = get_most_common(df_curr)
  )
  
  # Percent change from previous to current
  change_df <- merge(
    df_prev[, c("Mobility_Status", "Achievement_Level", "Percent")],
    df_curr[, c("Mobility_Status", "Achievement_Level", "Percent")],
    by = c("Mobility_Status", "Achievement_Level"),
    all = TRUE,
    suffixes = c("_Previous", "_Current")
  )
  change_df[is.na(change_df)] <- 0
  change_df$Percent_Change <- round(change_df$Percent_Current - change_df$Percent_Previous, 1)
  
  # Output list
  OUT <- list(
    Previous_Table = previous_table,
    Current_Table = current_table,
    Data = combined_df,
    Group_Sizes = group_sizes,
    Most_Common_Level = most_common,
    Change_Summary = change_df,
    Plot = cowplot::plot_grid(plot_prev, plot_curr, ncol = 2),
    Caption = paste(
      "Comparison of achievement by mobility status for students in Grade", current_grade - 1,
      "(", last_year_text, ") and Grade", current_grade, "(", current_year_text, ")."), 
    Note = "The 'Stay' group represents a true cohort of students observed in both years, while",
    "'Join' and 'Leave' represent those entering or exiting the cohort."
  )
  
  # Produce the output
  return(invisible(OUT))
}
