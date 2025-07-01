#' Analyze Cohort Persistence with Entry and Retention Tracking
#'
#' @title Analyze Cohort Persistence
#' @description
#' Computes student cohort persistence starting from a specified grade and year.
#' Tracks new students who enter each subsequent grade-year and their persistence.
#' Produces summary tables and several plots including line plots, heatmaps, 
#' enrollment plots, and retention dropoff plots.
#' 
#' Optionally allows subsetting the dataset prior to analysis.
#'
#' @param dataset Data frame containing at least student IDs, grades, and years.
#' @param start_grade Numeric or character; cohort start grade.
#' @param start_year Numeric or character; cohort start year (calendar year).
#' @param end_grade Optional numeric or character; last grade to include in analysis.
#'   Defaults to max grade found in the dataset.
#' @param subset_expr Optional expression for subsetting the dataset before analysis.
#'   This is evaluated in the context of the dataset (e.g., Gender == "Female").
#'
#' @return A named list with elements:
#' \describe{
#'   \item{Summary}{Data frame summarizing cohorts by join year/grade, year, grade, counts, and persistence percentages.}
#'   \item{Retention_Table}{Wide-format table of cohort persistence percentages by cohort and grade.}
#'   \item{Persistence_Data}{List of data frames for persistent student records by year.}
#'   \item{Entry_Data}{List of new entrants for each join cohort.}
#'   \item{Entry_Trajectories}{List of persistence trajectories for new entrants by join cohort.}
#'   \item{Line_Plot}{ggplot2 object showing cohort persistence over grades.}
#'   \item{Heatmap_Plot}{ggplot2 heatmap showing persistence percentages by years since join and cohort.}
#'   \item{Enrollment_Plot}{ggplot2 line plot of enrollment counts by grade and cohort.}
#'   \item{Enrollment_Barplot}{ggplot2 stacked bar plot of enrollment by academic year and cohort.}
#'   \item{Retention_Plot}{ggplot2 plot showing cohort retention dropoff over time.}
#'   \item{Statistics}{List of summary statistics including initial/final counts and retention rates.}
#' }
#'
#' @import ggplot2
#' @importFrom stats aggregate median na.omit
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' analyze_cohort_persistence(dataset = math, start_year = 2019, start_grade = 3, end_grade = 5)
#' 

analyze_cohort_persistence <- function(dataset, start_grade, start_year, end_grade = NULL, subset_expr = NULL) {
  # Subset dataset if subset_expr is provided
  if (!is.null(subset_expr)) {
    dataset <- subset(dataset, eval(substitute(subset_expr), dataset, parent.frame()))
  }
  
  # Check for required variables (case-insensitive)
  colnames_lower <- tolower(names(dataset))
  required_variables <- c("id", "grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Map actual column names
  name_map <- setNames(names(dataset), colnames_lower)
  id_col <- name_map["id"]
  grade_col <- name_map["grade"]
  year_col <- name_map["year"]
  
  # Normalize columns
  dataset[[grade_col]] <- .normalize_grade(dataset[[grade_col]])
  dataset[[year_col]] <- .parse_year(dataset[[year_col]])
  
  # Normalize inputs
  start_grade <- as.numeric(.normalize_grade(start_grade))
  start_year <- as.numeric(.parse_year(start_year))
  if (is.null(end_grade)) {
    end_grade <- max(dataset[[grade_col]], na.rm = TRUE)
  } else {
    end_grade <- as.numeric(.normalize_grade(end_grade))
  }
  
  # Sequences for grades and years
  grade_sequence <- seq(from = start_grade, to = end_grade)
  year_sequence <- seq(from = start_year, length.out = length(grade_sequence))
  N_years <- length(grade_sequence)
  
  # Extract data for each grade-year
  TEMP_DATA <- vector("list", N_years)
  for(i in seq_len(N_years)) {
    TEMP_DATA[[i]] <- dataset[
      dataset[[year_col]] == year_sequence[i] & 
        dataset[[grade_col]] == grade_sequence[i], 
    ]
  }
  
  # Persistence initialization
  N_PERSIST <- numeric(N_years)
  PERSIST <- vector("list", N_years - 1)
  cohort_summary <- data.frame(YEAR=numeric(0), GRADE=numeric(0), COUNT=numeric(0), 
                               PERCENT=numeric(0))
  
  # Initial cohort count
  N_PERSIST[1] <- nrow(TEMP_DATA[[1]])
  cohort_summary[1, ] <- c(year_sequence[1], grade_sequence[1], N_PERSIST[1], 100)
  
  # Stop if no initial students
  if (N_PERSIST[1] == 0) {
    stop("No students found in the initial cohort. Please check your input data.")
  }
  
  # IDs in initial cohort
  PERSIST_ID <- TEMP_DATA[[1]][[id_col]]
  
  # Persistence through years
  for(j in 2:N_years) {
    PERSIST_ID <- intersect(PERSIST_ID, TEMP_DATA[[j]][[id_col]])
    N_PERSIST[j] <- length(PERSIST_ID)
    if(N_PERSIST[j] == 0) break
    PERSIST[[j - 1]] <- TEMP_DATA[[j]][TEMP_DATA[[j]][[id_col]] %in% PERSIST_ID, ]
    cohort_summary[j, ] <- c(year_sequence[j], grade_sequence[j], N_PERSIST[j], 
                             N_PERSIST[j] / N_PERSIST[1] * 100)
  }
  
  # Track new joiners after initial cohort
  join_data_list <- list()
  join_trajectories <- list()
  seen_ids <- TEMP_DATA[[1]][[id_col]]
  
  for(j in 2:(N_years - 1)) {
    current_ids <- TEMP_DATA[[j]][[id_col]]
    new_ids <- setdiff(current_ids, seen_ids)
    if(length(new_ids) == 0) next
    seen_ids <- union(seen_ids, current_ids)
    retained_ids <- new_ids
    traj <- data.frame(year=numeric(0), grade=numeric(0), count=numeric(0), percent=numeric(0))
    for(k in j:N_years) {
      step_data <- TEMP_DATA[[k]]
      matched_ids <- intersect(retained_ids, step_data[[id_col]])
      retained_ids <- matched_ids
      traj[nrow(traj) + 1, ] <- c(year_sequence[k], grade_sequence[k], length(matched_ids),
                                  if(length(new_ids) > 0) (length(matched_ids) / length(new_ids)) * 100 else 0)
      if(length(matched_ids) == 0) break
    }
    join_trajectories[[length(join_trajectories) + 1]] <- list(
      join_year = year_sequence[j],
      join_grade = grade_sequence[j],
      initial_n = length(new_ids),
      trajectory = traj
    )
    join_data_list[[length(join_data_list) + 1]] <- list(
      join_year = year_sequence[j],
      join_grade = grade_sequence[j],
      join_ids = new_ids,
      persisted_ids = retained_ids
    )
  }
  
  # Most recent year joiners
  last_year_index <- N_years
  last_year_ids <- TEMP_DATA[[last_year_index]][[id_col]]
  previous_ids <- unlist(lapply(TEMP_DATA[1:(last_year_index - 1)], function(d) d[[id_col]]))
  most_recent_new_ids <- setdiff(last_year_ids, previous_ids)
  
  if(length(most_recent_new_ids) > 0) {
    join_trajectories[[length(join_trajectories) + 1]] <- list(
      join_year = year_sequence[last_year_index],
      join_grade = grade_sequence[last_year_index],
      initial_n = length(most_recent_new_ids),
      trajectory = data.frame(
        year = year_sequence[last_year_index],
        grade = grade_sequence[last_year_index],
        count = length(most_recent_new_ids),
        percent = 100
      )
    )
    join_data_list[[length(join_data_list) + 1]] <- list(
      join_year = year_sequence[last_year_index],
      join_grade = grade_sequence[last_year_index],
      join_ids = most_recent_new_ids,
      persisted_ids = most_recent_new_ids
    )
  }
  
  # Summary tables
  original_df <- data.frame(
    JOIN_YEAR = year_sequence[1],
    JOIN_GRADE = grade_sequence[1],
    YEAR = cohort_summary$YEAR,
    GRADE = cohort_summary$GRADE,
    COUNT = cohort_summary$COUNT,
    PERCENT = cohort_summary$PERCENT
  )
  join_df <- do.call(rbind, lapply(join_trajectories, function(x) {
    data.frame(
      JOIN_YEAR = x$join_year,
      JOIN_GRADE = x$join_grade,
      YEAR = x$trajectory$year,
      GRADE = x$trajectory$grade,
      COUNT = x$trajectory$count,
      PERCENT = x$trajectory$percent
    )
  }))
  combined_summary <- rbind(original_df, join_df)
  combined_summary <- combined_summary[order(combined_summary$JOIN_YEAR, combined_summary$JOIN_GRADE,
                                             combined_summary$YEAR, combined_summary$GRADE), ]
  
  combined_summary$YEAR_LABEL <- .to_academic_year(combined_summary$YEAR)
  combined_summary$JOIN_YEAR_LABEL <- .to_academic_year(combined_summary$JOIN_YEAR)
  combined_summary$Years_Since_Join <- combined_summary$YEAR - combined_summary$JOIN_YEAR
  combined_summary$Cohort_Label <- paste0("Grade ", combined_summary$JOIN_GRADE,
                                          " (", combined_summary$JOIN_YEAR_LABEL, ")")
  cohort_levels <- unique(combined_summary[order(combined_summary$JOIN_YEAR, 
                                                 combined_summary$JOIN_GRADE), "Cohort_Label"])
  combined_summary$Cohort_Label <- factor(combined_summary$Cohort_Label, levels = rev(cohort_levels))
  combined_summary$AcademicYear <- .to_academic_year(combined_summary$YEAR)
  
  # Output list
  OUT <- list()
  OUT$Summary <- combined_summary
  OUT$Persistence_Data <- PERSIST
  OUT$Entry_Data <- join_data_list
  OUT$Entry_Trajectories <- join_trajectories
  
  # Heatmap data prep
  heatmap_df <- combined_summary
  heatmap_df$Label <- ifelse(heatmap_df$Years_Since_Join == 0,
                             as.character(heatmap_df$COUNT),
                             paste0(round(heatmap_df$PERCENT), "%"))
  heatmap_df$FillValue <- ifelse(heatmap_df$Years_Since_Join == 0, NA, heatmap_df$PERCENT)
  heatmap_df$LabelColor <- ifelse(heatmap_df$Years_Since_Join == 0, "black", "white")
  
  # Enrollment summary for plots
  total_enrollment <- aggregate(COUNT ~ YEAR + GRADE, data = combined_summary, sum)
  total_enrollment$YEAR_LABEL <- .to_academic_year(total_enrollment$YEAR)
  
  # Enrollment Plot: lines for each cohort + total black line
  EnrollmentPlot <- ggplot() +
    geom_line(data = total_enrollment, aes(x = GRADE, y = COUNT, group = 1), 
              color = "black", linewidth = 1.2) +
    geom_point(data = total_enrollment, aes(x = GRADE, y = COUNT), color = "black", 
               size = 2) +
    geom_line(data = combined_summary, aes(x = GRADE, y = COUNT, group = Cohort_Label, 
                                           color = Cohort_Label), linewidth = 1) +
    geom_point(data = combined_summary, aes(x = GRADE, y = COUNT, color = Cohort_Label), 
               size = 2) +
    scale_x_continuous(breaks = seq(floor(min(total_enrollment$GRADE)), 
                                    ceiling(max(total_enrollment$GRADE))),
                       minor_breaks = NULL) +
    labs(title = "Enrollment by Grade and Cohort",
         x = "Grade", y = "Number of Students", color = "Join Cohort") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey80"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  OUT$Enrollment_Plot <- EnrollmentPlot
  
  # Line plot for persistence %
  LinePlot <- ggplot(combined_summary,
                     aes(x = GRADE, y = PERCENT,
                         group = interaction(JOIN_YEAR_LABEL, JOIN_GRADE),
                         color = paste0("Grade ", JOIN_GRADE, " (", JOIN_YEAR_LABEL, ")"))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(floor(min(combined_summary$GRADE)), 
                                    ceiling(max(combined_summary$GRADE))),
                       minor_breaks = NULL) +
    labs(title = "Cohort Persistence Over Time",
         x = "Grade", y = "Percent Retained", color = "Join Cohort") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(color = "grey80"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)) +
    ylim(0, 100)
  OUT$Line_Plot <- LinePlot
  
  # Heatmap plot
  HeatmapPlot <- ggplot(heatmap_df) +
    geom_raster(aes(x = Years_Since_Join, y = Cohort_Label, fill = FillValue), na.rm = TRUE) +
    geom_text(aes(x = Years_Since_Join, y = Cohort_Label, label = Label, color = LabelColor), 
              size = 4) +
    scale_color_identity() +
    scale_fill_gradient(low = "deepskyblue", high = "darkblue", na.value = "grey90", 
                        limits = c(0, 100)) +
    scale_x_continuous(breaks = 0:max(heatmap_df$Years_Since_Join), expand = c(0, 0)) +
    labs(title = paste0("Cohort Persistence Heatmap\nStarting Cohort: Grade ", 
                        start_grade, " (", .to_academic_year(start_year), ")"),
         x = "Years Since Join", y = "Join Cohort", fill = "Persistence %") +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank())
  OUT$Heatmap_Plot <- HeatmapPlot
  
  # Retention wide table
  retention_wide <- with(combined_summary, 
                         tapply(PERCENT, list(Cohort_Label, GRADE), FUN = function(x) x[1]))
  retention_wide <- as.data.frame.matrix(retention_wide)
  retention_wide <- cbind(Cohort_Label = rownames(retention_wide), retention_wide)
  retention_wide[-1] <- round(retention_wide[-1], 1)
  rownames(retention_wide) <- NULL
  OUT$Retention_Table <- retention_wide
  
  # Chronological levels (oldest first)
  chron_levels <- unique(combined_summary[order(combined_summary$JOIN_YEAR, combined_summary$JOIN_GRADE), "Cohort_Label"])
  
  # Create a reversed factor for stacking
  combined_summary$Cohort_Label_Stack <- factor(combined_summary$Cohort_Label, levels = rev(chron_levels))
  
  # Enrollment Bar Plot with Stacks for each Cohort
  EnrollmentBarPlot <- ggplot() +
    geom_bar(
      data = combined_summary,
      aes(x = AcademicYear, y = COUNT, fill = Cohort_Label_Stack),
      stat = "identity"
    ) +
    geom_text(
      data = combined_summary,
      aes(x = AcademicYear, y = COUNT, label = COUNT, group = Cohort_Label_Stack),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "white"
    ) +
    scale_fill_discrete(
      breaks = chron_levels,  # legend order chronological
      labels = chron_levels,
      guide = guide_legend(reverse = FALSE)
    ) +
    labs(
      title = "Enrollment by Grade and Cohort",
      x = "Academic Year",
      y = "Number of Students",
      fill = "Cohort"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  OUT$Enrollment_BarPlot <- EnrollmentBarPlot
  
  # Stats summary
  valid_counts <- N_PERSIST[N_PERSIST > 0]
  stats <- list(
    Initial_Count = N_PERSIST[1],
    Final_Count = tail(valid_counts, 1),
    Overall_Rate = (tail(valid_counts, 1) / N_PERSIST[1]) * 100,
    Average_Annual_Rate = mean(diff(valid_counts) / head(valid_counts, -1), na.rm = TRUE) * 100,
    Median_Annual_Rate = median(diff(valid_counts) / head(valid_counts, -1), na.rm = TRUE) * 100
  )
  OUT$Statistics <- stats
  
  # Retention dropoff plot
  dropoff_df <- data.frame(
    YearsSinceJoin = 0:(length(valid_counts) - 1),
    PercentRetained = (valid_counts / valid_counts[1]) * 100
  )
  RetentionDropPlot <- ggplot(dropoff_df, aes(x = YearsSinceJoin, y = PercentRetained)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "steelblue", size = 3) +
    scale_x_continuous(breaks = 0:max(dropoff_df$YearsSinceJoin)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 105)) +
    labs(title = "Cohort Retention Dropoff Over Time",
         x = "Years Since Join", y = "Percent Retained") +
    theme_minimal()
  OUT$Retention_Plot <- RetentionDropPlot
  
  # Provide the output
  return(invisible(OUT))
}
