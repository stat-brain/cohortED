#' Analyze Student Cohorts with Persistence Summary and Visualizations
#' 
#' @title Analyze Student Cohorts
#' 
#' @description
#' Identifies student cohorts based on their first observed grade and year,
#' then tracks their grade-level progression year over year. Students who do
#' not progress by one grade per year are dropped from the cohort at the point
#' of deviation.
#'
#' @param dataset A data.frame with at least `id`, `grade`, and `year`.
#' @param details Logical; if TRUE, returns additional internal components like the cleaned trajectory data and nested cohort list.
#' @param extra_variables Optional character vector of additional column names to carry through the output (e.g., demographics or school info).
#'
#' @return A list with:
#' \describe{
#'   \item{Trajectories}{Student-level data with cohort assignment}
#'   \item{Data}{Nested list of student IDs by cohort and year}
#'   \item{Summary}{Persistence summary table}
#'   \item{Heatmaps}{List of ggplot heatmaps, one per cohort}
#' }
#' 
#' @import ggplot2
#' @export
#' 

analyze_student_cohorts <- function(dataset, details = FALSE, extra_variables = NULL) {
  #--- Input validation ---
  if (!is.data.frame(dataset)) stop("'dataset' must be a data frame.")
  colnames_lower <- tolower(names(dataset))
  required_variables <- c("id", "grade", "year")
  missing <- required_variables[!required_variables %in% colnames_lower]
  if (length(missing) > 0) stop(paste("Missing required variable(s):", paste(missing, collapse = ", ")))
  
  #--- Standardize column names ---
  col_map <- setNames(names(dataset), tolower(names(dataset)))
  df <- dataset[, col_map[required_variables], drop = FALSE]
  names(df) <- c("student_id", "grade", "year")
  
  #--- Add extra_variables if specified and present ---
  if (!is.null(extra_variables)) {
    extra_variables_present <- extra_variables[tolower(extra_variables) %in% tolower(names(dataset))]
    if (length(extra_variables_present) > 0) {
      # Map exact names from dataset for case sensitivity
      dataset_names_lower <- tolower(names(dataset))
      extra_cols <- sapply(tolower(extra_variables_present), function(x) names(dataset)[dataset_names_lower == x])
      
      extra_data <- unique(dataset[, c(col_map["id"], extra_cols), drop = FALSE])
      names(extra_data)[1] <- "student_id"
      
      # Merge on student_id (one row per student assumed)
      df <- merge(df, extra_data, by = "student_id", all.x = TRUE)
    }
  }
  
  #--- Coerce year and grade ---
  df$year <- .parse_year(df$year)
  df$grade <- as.numeric(as.character(df$grade))
  
  #--- Identify join point for each student ---
  join_info <- df[order(df$student_id, df$year, df$grade), ]
  join_info <- join_info[!duplicated(join_info$student_id), ]
  join_info$join_year <- join_info$year
  join_info$join_grade <- join_info$grade
  join_info$cohort_label <- paste0("Grade", join_info$join_grade, "_", join_info$join_year)
  
  #--- Merge cohort info back into dataset ---
  df <- merge(df, join_info[, c("student_id", "join_year", "join_grade", "cohort_label")], by = "student_id")
  df <- df[order(df$student_id, df$year), ]
  
  #--- Determine valid linear grade progression ---
  df$Years_Since_Join <- df$year - df$join_year
  df$Expected_Grade <- df$join_grade + df$Years_Since_Join
  df$Valid <- df$grade == df$Expected_Grade
  
  #--- Drop students after first invalid year ---
  df <- do.call(rbind, lapply(split(df, df$student_id), function(subdf) {
    subdf$cumulative_valid <- cumprod(subdf$Valid) == 1
    subdf[subdf$cumulative_valid, ]
  }))
  
  #--- Build output ---
  OUT <- list()
  OUT$Trajectories <- df
  
  #--- Nested list of students by cohort and year ---
  cohort_list <- split(df, df$cohort_label)
  cohort_list <- lapply(cohort_list, function(subdf) {
    split(subdf$student_id, subdf$year)
  })
  OUT$Data <- cohort_list
  
  #--- Persistence summary ---
  summary_df <- aggregate(student_id ~ cohort_label + join_year + year + grade,
                          data = df,
                          FUN = function(x) length(unique(x)))
  names(summary_df)[names(summary_df) == "student_id"] <- "n_students"
  
  #--- Entry size per cohort ---
  entry_sizes <- summary_df[summary_df$join_year == summary_df$year,
                            c("cohort_label", "n_students")]
  names(entry_sizes)[2] <- "entry_size"
  summary_df <- merge(summary_df, entry_sizes, by = "cohort_label")
  summary_df$percent_retained <- round(100 * summary_df$n_students / summary_df$entry_size, 1)
  summary_df <- summary_df[order(summary_df$join_year, summary_df$grade), ]
  OUT$Summary <- summary_df
  
  #--- Generate combined heatmaps by join_year ---
  heatmaps_by_year <- list()
  unique_join_years <- sort(unique(df$join_year))
  
  for (jy in unique_join_years) {
    heatmap_df <- summary_df[summary_df$join_year == jy, ]
    
    # Calculate Years_Since_Join
    heatmap_df$Years_Since_Join <- heatmap_df$year - jy
    
    # Prepare labels & fill values
    heatmap_df$FillValue <- heatmap_df$percent_retained
    heatmap_df$Label <- paste0(heatmap_df$percent_retained, "%")
    heatmap_df$LabelColor <- "black"
    
    # For first column (join year), replace fill with a constant code (e.g., 101) and label with counts
    heatmap_df$FillValue[heatmap_df$Years_Since_Join == 0] <- 101
    heatmap_df$Label[heatmap_df$Years_Since_Join == 0] <- heatmap_df$n_students[heatmap_df$Years_Since_Join == 0]
    heatmap_df$LabelColor[heatmap_df$Years_Since_Join == 0] <- "black"
    
    # Fill in missing grade and years to complete the grid
    all_grades <- sort(unique(df$grade))
    all_years_since_join <- 0:max(heatmap_df$Years_Since_Join)
    full_grid <- expand.grid(Years_Since_Join = all_years_since_join,
                             grade = all_grades, stringsAsFactors = FALSE)
    
    plot_df <- merge(full_grid, heatmap_df, by = c("Years_Since_Join", "grade"), all.x = TRUE)
    
    # Fix missing labels for missing data
    plot_df$Label[is.na(plot_df$Label)] <- ""
    plot_df$FillValue[is.na(plot_df$FillValue)] <- NA
    plot_df$LabelColor[is.na(plot_df$LabelColor)] <- "black"
    
    # Create heatmap plot
    p <- ggplot(plot_df) +
      geom_raster(aes(x = Years_Since_Join, y = grade, fill = FillValue), na.rm = TRUE) +
      geom_text(aes(x = Years_Since_Join, y = grade, label = Label, color = LabelColor),
                size = 4, na.rm = TRUE) +
      scale_color_identity() +
      
      # Fill scale: tan for join year (coded as 101), continuous blue scale 0-100%
      scale_fill_gradientn(
        colors = c("#EBD8C1", "#80DFFF", "#7F89BF"),
        values = c(101, 0, 100) / 101,  # Manual rescale to [0, 1]
        limits = c(0, 101),
        na.value = "white",
        breaks = c(0, 25, 50, 75, 100),
        labels = c("0%", "25%", "50%", "75%", "100%"),
        name = "Persistence (%)"
      ) +
      scale_y_continuous(breaks = all_grades, expand = expansion(mult = c(0, 0))) +
      scale_x_continuous(breaks = all_years_since_join, expand = expansion(mult = c(0, 0))) +
      labs(
        title = paste0("Cohort Persistence Heatmap - Join Year: ", jy),
        x = "Years Since Join",
        y = "Grade"
      ) +
      
      theme_minimal(base_size = 14) +
      theme(panel.grid = element_blank())
    
    heatmaps_by_year[[as.character(jy)]] <- p
  }
  
  OUT$Heatmaps <- heatmaps_by_year
  
  return(invisible(OUT))
}

