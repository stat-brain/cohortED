#' Analyze Entry Type and Achievement Patterns in a Target Grade
#'
#' @description
#' Identifies how students entered a specified grade (e.g., stayed on track, repeated, skipped in, dropped back, joined) and summarizes their academic performance by year. 
#' Also identifies students who skipped over the grade entirely. Designed for use with the output of `summarize_grade_changes()`.
#'
#' @param grade_change_output A list returned from `summarize_grade_changes()`, including student-level transitions and classified subgroups.
#' @param grade A grade label (e.g., "5" or "K") to analyze.
#' @param achievement_var Character string specifying the column name representing student achievement. Must be present in `grade_change_output$Student_Transitions`.
#' @param n_proficiencies Number of top levels to count as proficient if `achievement_var` is categorical. Passed to `make_proficiency_levels()`.
#'
#' @return A named list with the following report-ready components:
#' 
#' \describe{
#'   \item{Target_Grade_Transitions_By_Year}{A wide-format table summarizing how students entered the grade (Stay, Repeat, Skip_In, Drop_Back, Join, Other) by academic year.}
#'   \item{Skipped_Over_Grade_By_Year}{A table showing the number of students who skipped over the target grade each year.}
#'   \item{Achievement_By_Transition_And_Year}{A table showing % proficient (and mean score if numeric) by entry type and academic year.}
#'   \item{Transition_Plot}{A stacked bar plot showing the number of students by entry type and year.}
#'   \item{Achievement_Plot}{A line plot showing changes in proficiency rates by entry type across years.}
#'   \item{Data}{A list of supporting data frames: Target_Grade_Students, Achievement_Data, Skipped_Over_Students, Join, Other.}
#'   \item{Notes}{A character vector describing the analysis context.}
#' }
#'
#' @details
#' Entry types are assigned based on grade-level change relative to expected yearly progression. For example:
#' - "Repeat": student was in the same grade two years in a row
#' - "Skip_In": student advanced more than one grade in a single year and is observed in the target grade
#' - "Drop_Back": student regressed more than one grade
#' - "Stay": student followed expected grade-level progression
#' - "Join": student entered the dataset in the target grade with no prior record
#' - "Other": unmatched or ambiguous patterns (e.g., mid-year transfers)
#'
#'
#' @import ggplot2
#' @export

analyze_grade_changes <- function(grade_change_output,
                                  grade,
                                  achievement_var = "ACHIEVEMENT_LEVEL",
                                  n_proficiencies = 3) {
  #--- Validate inputs ---
  if (!is.list(grade_change_output)) stop("grade_change_output must be the result of summarize_grade_changes().")
  if (!all(c("Student_Transitions", "Data") %in% names(grade_change_output))) {
    stop("grade_change_output must contain 'Student_Transitions' and 'Data'.")
  }
  
  #--- Pull student-level transitions and group data ---
  transitions <- grade_change_output$Student_Transitions
  group_data <- grade_change_output$Data
  
  #--- Normalize grade ---
  transitions$GRADE_NUM <- .normalize_grade(transitions$GRADE)
  target_num <- .normalize_grade(grade)
  
  #--- Get students in target grade by year ---
  tg_students <- transitions[transitions$GRADE_NUM == target_num, c("ID", "YEAR", "ACADEMIC_YEAR")]
  
  #--- Identify entry type ---
  get_ids <- function(group_name) {
    dat <- group_data[[group_name]]
    if (is.null(dat)) return(character(0))
    unique(as.character(dat$ID[.normalize_grade(dat$GRADE) == target_num]))
  }
  
  ids_repeat <- get_ids("Repeat")
  ids_skip   <- get_ids("Skip")
  ids_drop   <- get_ids("Drop_Back")
  ids_stay   <- get_ids("Stay")
  
  #--- Identify joins (no prior year in dataset) ---
  prior_years <- transitions$YEAR - 1
  join_ids <- tg_students$ID[!tg_students$ID %in% transitions$ID[transitions$YEAR %in% prior_years]]
  
  #--- Classify target grade students ---
  tg_students$Entry_Type <- ifelse(tg_students$ID %in% ids_repeat, "Repeat",
                                   ifelse(tg_students$ID %in% ids_skip, "Skip_In",
                                          ifelse(tg_students$ID %in% ids_drop, "Drop_Back",
                                                 ifelse(tg_students$ID %in% ids_stay, "Stay",
                                                        ifelse(tg_students$ID %in% join_ids, "Join", "Other")))))
  
  #--- Table of target grade entry types by year (wide + long for plotting) ---
  trans_table <- as.data.frame(table(tg_students$ACADEMIC_YEAR, tg_students$Entry_Type))
  names(trans_table) <- c("Academic_Year", "Entry_Type", "Count")
  trans_wide <- reshape(trans_table, idvar = "Academic_Year", timevar = "Entry_Type", direction = "wide")
  names(trans_wide) <- gsub("Count\\.", "", names(trans_wide))
  trans_wide[is.na(trans_wide)] <- 0
  trans_wide$Total <- rowSums(trans_wide[ , !(names(trans_wide) %in% "Academic_Year")])
  
  #--- Skipped over: students who never appeared in target grade ---
  all_ids <- unique(as.character(transitions$ID))
  ids_in_target <- unique(as.character(tg_students$ID))
  ids_skipped_over <- setdiff(all_ids, ids_in_target)
  skipped_df <- transitions[transitions$ID %in% ids_skipped_over, ]
  
  check_skipped <- function(df) {
    gseq <- df$GRADE_NUM
    any(gseq < target_num & target_num < max(gseq))
  }
  skipped_flags <- tapply(skipped_df, skipped_df$ID, check_skipped)
  skipped_ids <- names(skipped_flags)[skipped_flags]
  skipped_years <- transitions[transitions$ID %in% skipped_ids & transitions$GRADE_NUM > target_num, ]
  skip_by_year <- as.data.frame(table(skipped_years$ACADEMIC_YEAR))
  names(skip_by_year) <- c("Academic_Year", "N_Skipped_Over")
  
  #--- Join achievement data from transitions object ---
  ach <- transitions
  ach <- ach[ach$GRADE_NUM == target_num & !is.na(ach[[achievement_var]]), ]
  
  #--- Merge with entry type info ---
  tg_students_unique <- tg_students[!duplicated(tg_students[c("ID", "YEAR")]), ]
  ach <- merge(ach, tg_students_unique, by.x = c("ID", "YEAR"), by.y = c("ID", "YEAR"), all.x = FALSE)
  
  #--- Create achievement summary by entry type and year ---
  ach_summary <- do.call(rbind, lapply(split(ach, list(ach$ACADEMIC_YEAR, ach$Entry_Type)), function(subdat) {
    if (nrow(subdat) == 0) return(NULL)
    prof <- make_proficiency_levels(subdat, achievement = achievement_var,
                                    n_proficiencies = n_proficiencies, print_plot = FALSE)$Table
    out <- data.frame(
      Academic_Year = unique(subdat$ACADEMIC_YEAR),
      Entry_Type = unique(subdat$Entry_Type),
      N = nrow(subdat),
      Percent_Proficient = as.numeric(gsub("%", "", prof$Proficient[1]))
    )
    if (is.numeric(subdat[[achievement_var]])) {
      out$Mean_Score <- round(mean(subdat[[achievement_var]], na.rm = TRUE), 1)
    }
    return(out)
  }))
  rownames(ach_summary) <- NULL
  
  #--- Plots ---
  plot_transitions <- ggplot(trans_table, aes(x = Academic_Year, y = Count, fill = Entry_Type)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Entry Type Counts in Grade", grade), x = "Academic Year", y = "Number of Students") +
    theme_minimal()
  
  plot_achievement <- ggplot(ach_summary, aes(x = Academic_Year, y = Percent_Proficient, group = Entry_Type, color = Entry_Type)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(title = paste("Proficiency by Entry Type in Grade", grade), x = "Academic Year", y = "% Proficient") +
    theme_minimal()
  
  #--- Return output ---
  OUT <- list()
  OUT$Target_Grade_Transitions_By_Year <- trans_wide
  OUT$Skipped_Over_Grade_By_Year <- skip_by_year
  OUT$Achievement_By_Transition_And_Year <- ach_summary
  OUT$Transition_Plot <- plot_transitions
  OUT$Achievement_Plot <- plot_achievement
  OUT$Data <- list(
    Target_Grade_Students = tg_students,
    Achievement_Data = ach,
    Skipped_Over_Students = skipped_years,
    Join = tg_students[tg_students$Entry_Type == "Join", ],
    Other = tg_students[tg_students$Entry_Type == "Other", ]
  )
  OUT$Notes <- c(
    sprintf("Target grade analyzed: %s (numeric: %s)", grade, target_num),
    sprintf("Achievement variable: %s", achievement_var)
  )
  return(OUT)
}

