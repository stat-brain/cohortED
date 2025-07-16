#' @title Report Summary of Achievement by Student Mobility Status
#'
#' @description
#' Generates a narrative summary comparing student achievement across mobility groups
#' ("Stay", "Join", and "Leave") based on the output from `compare_achievement_mobility()`.
#' The summary highlights differences in proficiency rates, shifts in the most common
#' achievement levels, and within-group changes among students who remained in the same
#' school system across two years.
#'
#' @param mobility_output A named list returned by `compare_achievement_mobility()`, containing
#'   summary tables, percent values, and labels used to construct the narrative.
#' @param proficiency_levels Character vector of levels considered "Proficient or above".
#'   Default is `c("Proficient", "Advanced")`.
#' @param gap_threshold Numeric. Minimum percent difference to flag a group-level difference
#'   for use in visual or narrative comparisons (default = 5).
#' @param diff_threshold Numeric. Minimum percent difference to trigger a tailored interpretation
#'   of proficiency gaps in the narrative summary (default = 5).
#'
#' @return A named list containing narrative summaries for reporting:
#'
#' @export
#' 

report_achievement_mobility <- function(mobility_output, proficiency_levels = c("Proficient", "Advanced"), gap_threshold = 5, diff_threshold = 5) {
  # Extract key inputs
  df <- mobility_output$Data
  stay_summary <- mobility_output$Stay_Change_Summary
  change_table <- mobility_output$Achievement_Change_Summary
  most_common <- mobility_output$Most_Common_Level
  caption <- mobility_output$Caption
  
  # Parse years and grades from caption
  matches <- regmatches(caption, regexec("Grade (\\d+) \\(([^)]+)\\) and Grade (\\d+) \\(([^)]+)\\)", caption))[[1]]
  prior_grade <- matches[2]
  prior_year <- matches[3]
  current_grade <- matches[4]
  current_year <- matches[5]
  
  # Helper: percent proficient+advanced by group (current year only)
  current_df <- df[df$Grade == as.numeric(current_grade), ]
  previous_df <- df[df$Grade == as.numeric(prior_grade), ]
  leave_df <- df[df$Year == prior_year & df$Mobility_Status == "Leave" &
                   df$Achievement_Level %in% proficiency_levels, ]
  pa_summary1 <- aggregate(Percent ~ Mobility_Status, 
                          data = current_df[current_df$Achievement_Level %in% proficiency_levels, ],
                          sum)
  pa_summary2 <- aggregate(Percent ~ Mobility_Status, 
                           data = previous_df[previous_df$Achievement_Level %in% proficiency_levels, ],
                           sum)
  pa_map1 <- setNames(round(pa_summary1$Percent, 1), pa_summary1$Mobility_Status)
  pa_map2 <- setNames(round(pa_summary2$Percent, 1), pa_summary2$Mobility_Status)
  
  get_or_na <- function(x, name) if (name %in% names(x)) x[name] else NA_real_
  
  pa_stay1 <- get_or_na(pa_map1, "Stay")
  pa_join  <- get_or_na(pa_map1, "Join")
  pa_stay2 <- get_or_na(pa_map2, "Stay")
  pa_leave <- get_or_na(pa_map2, "Leave")
  
  # Change in Stay group
  improved <- stay_summary$Percent[stay_summary$Change == "Improved"]
  declined <- stay_summary$Percent[stay_summary$Change == "Declined"]
  no_change <- stay_summary$Percent[stay_summary$Change == "No Change"]
  
  # Common levels
  common_prev <- most_common$Previous
  common_curr <- most_common$Current
  
  # Compute differences relative to 'Stay' group
  diff_join <- pa_stay1 - pa_join
  diff_leave <- pa_stay2 - pa_leave
  
  # Tailored interpretation
  if (is.na(diff_join) || is.na(diff_leave)) {
    perf_msg <- "Insufficient data to compare performance across mobility groups."
  } else if (diff_join >= diff_threshold && diff_leave >= diff_threshold) {
    perf_msg <- "Students who remained in the same school system showed higher rates of proficiency and advanced achievement compared to those who joined or left during this period."
  } else if (diff_join <= -diff_threshold && diff_leave <= -diff_threshold) {
    perf_msg <- "Interestingly, students who joined or left the system outperformed those who stayed, suggesting potential shifts in the composition of the cohort."
  } else if (abs(diff_join) < diff_threshold && abs(diff_leave) < diff_threshold) {
    perf_msg <- "Achievement levels were relatively similar across mobility groups, with no substantial differences between students who stayed, joined, or left."
  } else if (diff_join >= diff_threshold && abs(diff_leave) < diff_threshold) {
    perf_msg <- "Students who stayed performed better than those who joined the school system, while students who left performed similarly to those who remained."
  } else if (diff_leave >= diff_threshold && abs(diff_join) < diff_threshold) {
    perf_msg <- "Students who stayed performed better than those who left the system, while joiners performed similarly to those who remained."
  } else if (diff_join <= -diff_threshold && abs(diff_leave) < diff_threshold) {
    perf_msg <- "Students who joined the system outperformed those who stayed, while those who left performed similarly to stayers."
  } else if (diff_leave <= -diff_threshold && abs(diff_join) < diff_threshold) {
    perf_msg <- "Students who left the system outperformed those who stayed, while joiners performed similarly."
  } else {
    perf_msg <- "There were differences in achievement across mobility groups, though no clear pattern emerged across all groups."
  }
  
  stay_trend_msg <- if (!is.na(pa_stay1) && !is.na(pa_stay2)) {
    sprintf(
      "Among students who stayed, the percent scoring at or above proficient %s from %.1f%% in %s to %.1f%% in %s.",
      if (pa_stay1 > pa_stay2) "increased" else if (pa_stay1 < pa_stay2) "decreased" else "remained the same",
      pa_stay2, prior_year, pa_stay1, current_year
    )
  } else {
    NULL
  }
  
  # Final summary paragraph
  summary <- paste(
    "This analysis examines student achievement across mobility groups, focusing on a cohort that progressed",
    "from Grade", prior_grade, "in", prior_year, "to Grade", current_grade, "in", current_year, ".",
    "The 'Stay' group represents the same students observed in both years.",
    perf_msg
  )
  
  # Detect largest difference
  largest_gap <- max(abs(diff_join), abs(diff_leave))
  
  # Determine proficiency comparison sentence
  if (diff_join >= gap_threshold && diff_leave >= gap_threshold) {
    group_compare <- paste(
      "The 'Stay' group had higher proficiency rates in both years:",
      "current year (", sprintf("%.1f%%", pa_stay1), ") compared to joiners (", sprintf("%.1f%%", pa_join), "), and",
      "prior year (", sprintf("%.1f%%", pa_stay2), ") compared to leavers (", sprintf("%.1f%%", pa_leave), ").",
      "These gaps suggest potential academic disruption associated with student mobility."
    )
  } else if (diff_join >= gap_threshold && abs(diff_leave) < gap_threshold) {
    group_compare <- paste(
      "Students who stayed (", sprintf("%.1f%%", pa_stay1), ") performed better than those who joined (", sprintf("%.1f%%", pa_join), "),",
      "while those who left (", sprintf("%.1f%%", pa_leave), ") performed similarly to stayers in the prior year."
    )
  } else if (diff_leave >= gap_threshold && abs(diff_join) < gap_threshold) {
    group_compare <- paste(
      "Students who stayed (", sprintf("%.1f%%", pa_stay2), ") outperformed those who left (", sprintf("%.1f%%", pa_leave), ") in the prior year,",
      "while joiners (", sprintf("%.1f%%", pa_join), ") performed similarly to stayers in the current year."
    )
  } else if (abs(diff_join) < gap_threshold && abs(diff_leave) < gap_threshold) {
    group_compare <- paste(
      "Proficiency rates were similar across groups:",
      "Stay (", sprintf("%.1f%%", pa_stay1), "), Join (", sprintf("%.1f%%", pa_join), "), and Leave (", sprintf("%.1f%%", pa_leave), ")."
    )
  } else if (diff_join <= -gap_threshold && diff_leave <= -gap_threshold) {
    group_compare <- paste(
      "Interestingly, both joiners (", sprintf("%.1f%%", pa_join), ") and leavers (", sprintf("%.1f%%", pa_leave), 
      ") had higher proficiency rates than those who stayed (", sprintf("%.1f%%", pa_stay1), " in current year, and ", 
      sprintf("%.1f%%", pa_stay2), " in prior year),",
      "which may indicate a shift in the composition or needs of the remaining cohort."
    )
  } else {
    group_compare <- paste(
      "Proficiency rates varied across groups, with Stay at", sprintf("%.1f%%", pa_stay1), 
      "this year and", sprintf("%.1f%%", pa_stay2), "last year,",
      "Join at", sprintf("%.1f%%", pa_join), ", and Leave at", sprintf("%.1f%%", pa_leave), "."
    )
  }
  
  # Directional summary of Stay group
  if (improved > declined + 10) {
    stay_change_msg <- "A majority of students in the 'Stay' group improved their achievement from the prior year,"
  } else if (declined > improved + 10) {
    stay_change_msg <- "A noticeable share of 'Stay' students saw declines in achievement,"
  } else {
    stay_change_msg <- "Among 'Stay' students, gains and declines in achievement were fairly balanced,"
  }
  
  # Final detailed paragraph
  detailed <- paste(
    "Among students who remained in the system from Grade", prior_grade, "(", prior_year, ") to Grade",
    current_grade, "(", current_year, "),", stay_change_msg,
    "with", sprintf("%.1f%%", improved), "improving,",
    sprintf("%.1f%%", declined), "declining, and", sprintf("%.1f%%", no_change), "experiencing no change.",
    stay_trend_msg,
    "The most common achievement level in", prior_year, "was",
    tolower(common_prev[common_prev$Mobility_Status == "Stay", "Achievement_Level"]), ",",
    "shifting to",
    tolower(common_curr[common_curr$Mobility_Status == "Stay", "Achievement_Level"]), "in", current_year, ".",
    group_compare
  )
  
  
  # Supporting details
  parts <- c(
    if (!is.na(improved) && improved > 0) paste0(sprintf("%.1f%%", improved), " improved"),
    if (!is.na(no_change) && no_change > 0) paste0(sprintf("%.1f%%", no_change), " had no change"),
    if (!is.na(declined) && declined > 0) paste0(sprintf("%.1f%%", declined), " declined")
  )
  stay_change <- paste("Among 'Stay' students (same cohort from both years),", 
                       paste(parts, collapse = ", "), ".")
  
  # Dynamic narrative for proficiency comparison
  proficiency_compare <- paste(
    "In", current_year, ",", sprintf("%.1f%%", pa_stay1), "of students who stayed scored at or above proficient,",
    "compared to", sprintf("%.1f%%", pa_join), "among those who joined and",
    sprintf("%.1f%%", pa_leave), "among those who left.",
    if (diff_join >= gap_threshold && diff_leave >= gap_threshold) {
      "These differences suggest that mobility may be associated with lower academic performance."
    } else if (diff_join >= gap_threshold && abs(diff_leave) < gap_threshold) {
      "Students who joined performed notably below the 'Stay' group, while those who left performed similarly."
    } else if (diff_leave >= gap_threshold && abs(diff_join) < gap_threshold) {
      "Students who left scored lower than those who stayed, but joiners performed comparably."
    } else if (abs(diff_join) < gap_threshold && abs(diff_leave) < gap_threshold) {
      "Overall, proficiency rates were similar across all mobility groups."
    } else if (diff_join <= -gap_threshold && diff_leave <= -gap_threshold) {
      "Interestingly, both joiners and leavers outperformed the students who remained in the system."
    } else {
      "Proficiency differences were present but not consistent across groups."
    }
  )
  
  
  # Extract most common levels
  level_prev <- common_prev[common_prev$Mobility_Status == "Stay", "Achievement_Level"]
  level_curr <- common_curr[common_curr$Mobility_Status == "Stay", "Achievement_Level"]
  
  # Sentence logic
  if (!is.na(level_prev) && !is.na(level_curr) && level_prev != level_curr) {
    common_levels <- paste(
      "Among 'Stay' students, the most common achievement level shifted from",
      tolower(level_prev), "in", prior_year,
      "to", tolower(level_curr), "in", current_year, "."
    )
  } else {
    common_levels <- paste(
      "The most common performance level among 'Stay' students remained",
      tolower(level_curr), "in both", prior_year, "and", current_year, "."
    )
  }
  
  # Determine which plots/tables to recommend based on patterns
  note <- if (diff_join >= gap_threshold || diff_leave >= gap_threshold) {
    "Review the comparison plot to explore differences in achievement by mobility group. The 'Stay_Change_Plot' may also provide insight into within-group shifts over time."
  } else if (abs(diff_join) < gap_threshold && abs(diff_leave) < gap_threshold) {
    "Review the 'Stay_Change_Plot' for a closer look at how individual students' achievement changed over time, since group differences were minimal."
  } else {
    "Explore both the comparison plot and 'Stay_Change_Plot' to understand nuanced patterns of performance across and within groups."
  }
  
  # Start output list
  OUT <- list()
  
  # Add dynamically generated content
  OUT$Paragraph_Summary <- summary
  OUT$Paragraph_Detailed <- detailed
  OUT$Stay_Cohort_Change <- stay_change
  OUT$Proficiency_By_Group <- proficiency_compare
  OUT$Most_Common_Levels <- common_levels
  OUT$Note <- note
  
  # Return invisibly
  return(invisible(OUT))
}
