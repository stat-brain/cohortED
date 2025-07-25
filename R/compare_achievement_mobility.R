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
#' @param achievement_variable A character string naming the column that contains achievement levels (e.g., \code{"ACHIEVEMENT_LEVEL"} or \code{"PROF_LVL"}). Levels are automatically detected and ordered.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{\code{Previous_Table}}{A matrix of percent proficient by mobility group from the previous school year.}
#'   \item{\code{Current_Table}}{A matrix of percent proficient by mobility group from the current school year.}
#'   \item{\code{Data}}{A combined long-format data frame of achievement level percentages by mobility group across both years.}
#'   \item{\code{Group_Sizes}}{A list of data frames showing the number of students per mobility group for each year.}
#'   \item{\code{Most_Common_Level}}{A list of data frames showing the most common achievement level for each mobility group in each year.}
#'   \item{\code{Achievement_Change_Summary}}{A data frame showing the percent change in each achievement level by mobility group across years.}
#'   \item{\code{Stay_Change_Summary}}{A data frame showing how "Stay" students' achievement changed (Improved, No Change, Declined).}
#'   \item{\code{Comparison_Plot}}{A side-by-side plot showing separate bar charts for each year, grouped by mobility status and achievement level.}
#'   \item{\code{Achievement_Distribution_Plot}}{A grouped bar chart showing achievement level distributions across four categories: Previous/Leave, Previous/Stay, Current/Stay, and Current/Join.}
#'   \item{\code{Stay_Change_Plot}}{A bar plot showing the direction of change for "Stay" students.}
#'   \item{\code{Caption}}{A character string describing the comparison.}
#'   \item{\code{Note}}{A short note clarifying that "Stay" represents a true cohort; "Join" and "Leave" are not tracked longitudinally.}
#' }
#'
#' @details
#' Internally calls \code{\link{plot_alluvial_mobility}} to construct matched student data across years.
#' Achievement levels are dynamically detected from the specified column. Students with missing or
#' "No Score" achievement values are excluded from analysis.
#'
#' @importFrom ggplot2 ggplot aes geom_bar labs scale_fill_manual theme_minimal
#' @importFrom cowplot plot_grid
#' @importFrom stats xtabs
#' @importFrom grDevices colorRampPalette
#' @export
#'
#' @examples
#' compare_achievement_mobility(
#'   dataset = math,
#'   current_year = "2023-2024",
#'   current_grade = 5,
#'   achievement_variable = "ACHIEVEMENT_LEVEL"
#' )

compare_achievement_mobility <- function(dataset, current_year, current_grade, achievement_variable) {
  # Standardize column for internal use
  dataset$ACHIEVEMENT_LEVEL <- dataset[[achievement_variable]]
  
  # Get ordered levels dynamically
  achievement_levels <- levels(factor(dataset$ACHIEVEMENT_LEVEL))
  
  # Get student-level data with mobility and achievement levels
  NEW_DATA <- plot_alluvial_mobility(
    dataset = dataset,
    current_year = current_year,
    current_grade = current_grade,
    print_plot = FALSE
  )$Data
  
  # Using an internal function to handle current year
  current_year_num <- parse_year(current_year)
  last_year_text <- to_academic_year(current_year_num - 1)
  current_year_text <- to_academic_year(current_year_num)
  
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
  
  # Build ordered color palette from low (red) to high (green)
  achievement_palette <- colorRampPalette(c("#B22222", "#E57373", "#CCCCCC", "#66CDAA", "#2E8B57"))(length(achievement_levels))
  names(achievement_palette) <- achievement_levels
  
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
    scale_fill_manual(values = achievement_palette)
  
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
    scale_fill_manual(values = achievement_palette)
  
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
    Previous = .get_most_common(df_prev),
    Current = .get_most_common(df_curr)
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
  
  # Stay group change analysis
  stay_data <- NEW_DATA[NEW_DATA$mobility_status == "Stay", ]
  
  # Ensure levels match provided order
  stay_data$achievement_level.x <- factor(stay_data$achievement_level.x, levels = achievement_levels)
  stay_data$achievement_level.y <- factor(stay_data$achievement_level.y, levels = achievement_levels)
  
  # Remove cases with missing or "No Score"
  stay_data <- stay_data[!is.na(stay_data$achievement_level.x) &
                           !is.na(stay_data$achievement_level.y) &
                           stay_data$achievement_level.x != "No Score" &
                           stay_data$achievement_level.y != "No Score", ]
  
  # Compute change direction
  stay_data$change_direction <- factor(sign(
    as.integer(stay_data$achievement_level.y) - as.integer(stay_data$achievement_level.x)
  ),
  levels = c(-1, 0, 1),
  labels = c("Declined", "No Change", "Improved"))
  
  # Summary table of changes
  stay_change_summary <- as.data.frame(table(stay_data$change_direction))
  names(stay_change_summary) <- c("Change", "Count")
  stay_change_summary$Percent <- round(100 * stay_change_summary$Count / sum(stay_change_summary$Count), 1)
  
  
  # The Change Plot for "Stay" Students
  stay_change_plot <- ggplot(stay_change_summary, aes(x = Change, y = Percent, fill = Change)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5) +
    scale_fill_manual(values = c("Improved" = "#2E8B57", "No Change" = "#CCCCCC", "Declined" = "#B22222")) +
    labs(
      title = "Achievement Change for 'Stay' Students",
      y = "Percent of Students",
      x = "Direction of Change"
    ) +
    theme_minimal()
  
  #--- Add group labels for plotting ---
  df_prev$Group <- paste0("Previous_", df_prev$Mobility_Status)
  df_curr$Group <- paste0("Current_", df_curr$Mobility_Status)
  
  combined_plot_df <- rbind(df_prev, df_curr)
  combined_plot_df$Group <- factor(combined_plot_df$Group,
                                   levels = c("Previous_Leave", "Previous_Stay", "Current_Stay", "Current_Join"))
  
  # Side-by-side bar plot
  achievement_distribution_plot <- ggplot(combined_plot_df, aes(x = Achievement_Level, y = Percent, fill = Group)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
    scale_fill_manual(values = c(
      "Previous_Leave" = "#f4a582",
      "Previous_Stay" = "#92c5de",
      "Current_Stay" = "#0571b0",
      "Current_Join" = "#ca0020"
    )) +
    labs(
      title = "Achievement Level Distribution by Mobility Group",
      x = "Achievement Level",
      y = "Percent of Students",
      fill = "Group"
    ) +
    theme_minimal()
  
  
  # Output list
  OUT <- list(
    Previous_Table = previous_table,
    Current_Table = current_table,
    Data = combined_df,
    Group_Sizes = group_sizes,
    Most_Common_Level = most_common,
    Achievement_Change_Summary = change_df,
    Comparison_Plot = cowplot::plot_grid(plot_prev, plot_curr, ncol = 2),
    Achievement_Distribution_Plot = achievement_distribution_plot,
    Stay_Change_Plot = stay_change_plot, 
    Stay_Change_Summary = stay_change_summary,
    Caption = paste(
      "Comparison of achievement by mobility status for students in Grade", current_grade - 1,
      "(", last_year_text, ") and Grade", current_grade, "(", current_year_text, ")."), 
    Note = paste(
      "The 'Stay' group represents a true cohort of students observed in both years,",
      "'Join' and 'Leave' represent those entering or exiting the cohort."
    )
  )
  
  # Produce the output
  return(invisible(OUT))
}
