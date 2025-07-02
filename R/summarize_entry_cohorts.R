#' @title Summarize Entry Cohorts
#'
#' @description
#' Summarizes the characteristics and duration-in-cohort of student entry cohorts.
#' Returns demographic breakdowns, school-level group summaries, and duration histograms.
#'
#' @param trajectories A data frame, specifically the `$Trajectories` output from `analyze_student_cohorts()`.
#' @param demographic_variables Optional character vector specifying which demographic variables to include.
#'   If NULL, defaults to a predefined list of common student variables.
#' @return A list with:
#'   \describe{
#'     \item{GroupSummaries}{List of data frames summarizing demographic/school characteristics per cohort}
#'     \item{DurationTable}{Data frame with summary statistics on duration-in-cohort}
#'     \item{DurationHistograms}{List of ggplot histogram objects per cohort}
#'   }
#'
#' @importFrom stats aggregate ave
#' @import ggplot2
#' @export
#' 

summarize_entry_cohorts <- function(trajectories, demographic_variables = NULL) {
  # Validate input
  if (!is.data.frame(trajectories)) stop(
    "'trajectories' must be a data frame.\n",
    "Please run 'analyze_student_cohorts()' first and pass its $Trajectories output to this function."
  )
  
  required_variables <- c("student_id", "grade", "year", "join_year", "join_grade", 
                     "cohort_label", "Years_Since_Join")
  missing <- required_variables[!required_variables %in% names(trajectories)]
  if (length(missing) > 0) stop(
    "Missing required variable(s) in trajectories: ", paste(missing, collapse = ", "), ".\n",
    "Ensure you use the $Trajectories output from 'analyze_student_cohorts()'."
  )
  
  OUT <- list()
  
  join_only <- trajectories[trajectories$Years_Since_Join == 0, ]
  
  default_demo_variables <- c("GENDER", "ETHNICITY", "FREE_REDUCED_LUNCH_STATUS", "ELL_STATUS",
                         "IEP_STATUS", "GIFTED_AND_TALENTED_PROGRAM_STATUS", "SCHOOL_NUMBER",
                         "EMH_LEVEL", "DISTRICT_NUMBER")
  
  user_variables <- demographic_variables
  if (is.null(user_variables)) user_variables <- default_demo_variables
  
  demo_variables_present <- user_variables[tolower(user_variables) %in% tolower(names(trajectories))]
  demo_col_map <- setNames(names(trajectories), tolower(names(trajectories)))
  
  OUT$GroupSummaries <- lapply(demo_variables_present, function(var) {
    var_col <- demo_col_map[tolower(var)]
    tab <- aggregate(join_only$student_id, 
                     by = list(join_only$cohort_label, join_only[[var_col]]),
                     FUN = length)
    names(tab) <- c("Cohort", var, "Count")
    tab$Percent <- round(100 * tab$Count / ave(tab$Count, tab$Cohort, FUN = sum), 1)
    tab
  })
  names(OUT$GroupSummaries) <- demo_variables_present
  
  duration_df <- aggregate(Years_Since_Join ~ cohort_label + student_id, data = trajectories, FUN = max)
  duration_summary <- aggregate(Years_Since_Join ~ cohort_label, data = duration_df, 
                                FUN = function(x) c(mean = mean(x), median = median(x), 
                                                    max = max(x), n = length(x)))
  duration_table <- do.call(data.frame, duration_summary)
  names(duration_table) <- c("Cohort", "Mean_Years", "Median_Years", "Max_Years", "N")
  duration_table$Mean_Years <- round(duration_table$Mean_Years, 1)
  
  duration_table$join_grade <- as.numeric(gsub("Grade(\\d+)_.*", "\\1", duration_table$Cohort))
  duration_table$join_year <- as.numeric(gsub(".*_(\\d{4})", "\\1", duration_table$Cohort))
  duration_table <- duration_table[order(duration_table$join_grade, duration_table$join_year), ]
  duration_table$join_grade <- NULL
  duration_table$join_year <- NULL
  
  OUT$DurationTable <- duration_table
  
  ordered_cohorts <- duration_table$Cohort
  
  OUT$DurationHistograms <- lapply(ordered_cohorts, function(cohort_label) {
    subdf <- duration_df[duration_df$cohort_label == cohort_label, ]
    
    percent_df <- as.data.frame(table(subdf$Years_Since_Join))
    names(percent_df) <- c("Years_Since_Join", "Count")
    percent_df$Years_Since_Join <- as.numeric(as.character(percent_df$Years_Since_Join))
    percent_df$Percent <- 100 * percent_df$Count / sum(percent_df$Count)
    
    join_grade <- gsub("Grade(\\d+)_.*", "\\1", cohort_label)
    join_year <- gsub(".*_(\\d{4})", "\\1", cohort_label)
    
    title_text <- paste0("Entry: Grade ", join_grade, " in ", .to_academic_year(as.numeric(join_year)))
    
    ggplot(percent_df, aes(x = Years_Since_Join, y = Percent)) +
      geom_col(fill = "steelblue") +
      scale_x_continuous(breaks = 0:max(percent_df$Years_Since_Join)) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(title = title_text, x = "Years in Cohort", y = "Percent") +
      theme_minimal()
  })
  names(OUT$DurationHistograms) <- ordered_cohorts
  
  return(invisible(OUT))
}
