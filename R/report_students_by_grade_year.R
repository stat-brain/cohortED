#' @title Report Summary of Enrollment Trends by Grade and Year
#'  
#' @description
#' This function analyzes enrollment summary data to produce clear, narrative
#' paragraphs describing overall enrollment trends, grade-level changes, and
#' unusual patterns over time. It highlights key shifts, extreme years, and
#' grade-to-grade enrollment transitions in plain language, suitable for
#' reports or presentations to school boards and state agencies.
#'
#' @param summary_output A list output from \code{summarize_students_by_grade_year()} containing:
#'   \itemize{
#'     \item \code{Summary_Long}: data frame with detailed enrollment counts by grade and year,
#'     \item \code{Total_Enrollment}: data frame with total enrollment counts per year,
#'     \item \code{Grade_Statistics}: data frame with enrollment means and SDs per grade.
#'   }
#' @param pct_change_threshold Numeric scalar (default 10). Minimum percent change in total enrollment
#'   from the previous year considered noteworthy for summary.
#' @param grade_transition_threshold Numeric scalar (default 20). Threshold percent change in grade-to-grade
#'   enrollment used to flag notable shifts and highlight grades with largest increases or decreases.
#' @param zscore_threshold Numeric scalar (default 2). Z-score threshold for detecting unusual enrollment
#'   counts (outliers) within grades over years.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{Paragraph_Summary}}{A concise executive-style summary paragraph highlighting
#'       overall enrollment patterns and notable trends in simple language (no numbers).}
#'     \item{\code{Paragraph_Detailed}}{A detailed paragraph describing recent enrollment,
#'       grade-level changes, outliers, and variability with specific figures.}
#'     \item{\code{Enrollment_Trends}}{Sentence describing total enrollment in the most recent year and change from prior year.}
#'     \item{\code{Grade_Level_Changes}}{Sentence summarizing average and largest grade-to-grade enrollment changes.}
#'     \item{\code{Outlier_Summary}}{Summary of detected unusual enrollment levels by grade and year, if any.}
#'     \item{\code{Note}}{Suggested plot or table to review next based on findings (e.g., line plot, heatmap).}
#'   }
#'
#' @details
#' This function assists with interpreting enrollment data by automatically
#' generating textual summaries based on numeric thresholds. It identifies:
#' \itemize{
#'   \item Whether total enrollment has changed substantially recently.
#'   \item If the most recent year is an extreme (highest or lowest) enrollment year.
#'   \item Average and largest grade-to-grade changes in enrollment counts.
#'   \item Grades with consistently high enrollment relative to the overall mean.
#'   \item Grades and years with unusually high or low enrollment detected by z-score.
#'   \item Whether changes are generally consistent across grades or if some grades
#'     show more pronounced shifts.
#' }
#' It produces a brief, easy-to-understand summary paragraph for general audiences
#' and a more detailed paragraph with specific numeric insights for technical readers.
#'
#' The \code{Note} element recommends which visualizations or tables to review next,
#' such as line plots, bar plots, or heatmaps, to facilitate data exploration.
#'
#' @importFrom stats sd
#'
#' @seealso \code{\link{summarize_students_by_grade_year}}
#'
#' @export
#' 

report_students_by_grade_year <- function(
    summary_output,
    pct_change_threshold = 10,
    grade_transition_threshold = 20,
    zscore_threshold = 2
) {
  # Validate input
  required_names <- c("Summary_Long", "Total_Enrollment", "Grade_Statistics")
  missing <- setdiff(required_names, names(summary_output))
  if (length(missing) > 0) {
    stop("Input must include: ", paste(missing, collapse = ", "),
         ". You may need to run summarize_students_by_grade_year().")
  }
  
  long_table <- summary_output$Summary_Long
  total_table <- summary_output$Total_Enrollment
  grade_stats <- summary_output$Grade_Statistics
  
  OUT <- list()
  
  # --- Total Enrollment Trends ---
  total_table$PctChange <- c(
    NA,
    round(100 * diff(total_table$Count) / total_table$Count[-nrow(total_table)], 1)
  )
  
  recent_year <- max(total_table$YEAR)
  recent_label <- total_table$YEAR_LABEL[total_table$YEAR == recent_year]
  recent_enroll <- total_table$Count[total_table$YEAR == recent_year]
  prev_year <- recent_year - 1
  prev_enroll <- total_table$Count[total_table$YEAR == prev_year]
  pct_change_recent <- if (!is.na(prev_enroll)) {
    round(100 * (recent_enroll - prev_enroll) / prev_enroll, 1)
  } else NA_real_
  
  max_enroll <- max(total_table$Count, na.rm = TRUE)
  min_enroll <- min(total_table$Count, na.rm = TRUE)
  recent_is_max <- recent_enroll == max_enroll
  recent_is_min <- recent_enroll == min_enroll
  
  recent_enrollment_sentence <- paste0(
    "In ", recent_label, ", total enrollment was ", recent_enroll,
    if (!is.na(pct_change_recent)) {
      paste0(", a ", ifelse(pct_change_recent >= 0, "increase", "decrease"),
             " of ", abs(pct_change_recent), "% from the previous year.")
    } else "."
  )
  
  recent_extreme_sentence <- ""
  if (recent_is_max) {
    recent_extreme_sentence <- paste0(recent_label, " had the highest enrollment on record.")
  } else if (recent_is_min) {
    recent_extreme_sentence <- paste0(recent_label, " had the lowest enrollment on record.")
  }
  
  # --- Grade-to-Grade Changes ---
  grades <- sort(unique(long_table$GRADE_NUMERIC))
  prev_year_data <- subset(long_table, YEAR == prev_year)
  recent_year_data <- subset(long_table, YEAR == recent_year)
  
  grade_changes <- data.frame()
  for (g in grades) {
    from_grade <- g - 1
    to_grade <- g
    count_prev <- prev_year_data$Count[prev_year_data$GRADE_NUMERIC == from_grade]
    count_curr <- recent_year_data$Count[recent_year_data$GRADE_NUMERIC == to_grade]
    grade_label <- recent_year_data$GRADE[recent_year_data$GRADE_NUMERIC == to_grade]
    
    if (length(count_prev) == 1 && length(count_curr) == 1 && count_prev > 0) {
      pct_diff <- 100 * (count_curr - count_prev) / count_prev
      grade_changes <- rbind(grade_changes, data.frame(
        Grade = grade_label,
        PctChange = pct_diff
      ))
    }
  }
  
  if (nrow(grade_changes) > 0) {
    avg_change <- mean(abs(grade_changes$PctChange), na.rm = TRUE)
    max_change_row <- grade_changes[which.max(abs(grade_changes$PctChange)), ]
    grade_change_sentence <- paste0(
      "On average, enrollment by grade changed about ", round(avg_change, 1),
      "% compared to the prior grade the year before. The largest change was in grade ",
      max_change_row$Grade, " with a ", round(max_change_row$PctChange, 1), "% ",
      ifelse(max_change_row$PctChange > 0, "increase", "decrease"), "."
    )
  } else {
    avg_change <- 0
    grade_change_sentence <- "Grade-level enrollment remained stable compared to the prior year."
  }
  
  # --- Outlier Detection ---
  outlier_sentences <- c()
  for (g in grades) {
    grade_data <- subset(long_table, GRADE_NUMERIC == g)
    stats_row <- grade_stats[grade_stats$GRADE_NUMERIC == g, ]
    if (nrow(stats_row) == 0 || is.na(stats_row$SD) || stats_row$SD == 0) next
    z_scores <- (grade_data$Count - stats_row$MEAN) / stats_row$SD
    for (i in seq_along(z_scores)) {
      if (abs(z_scores[i]) >= zscore_threshold) {
        direction <- ifelse(z_scores[i] > 0, "higher", "lower")
        outlier_sentences <- c(outlier_sentences, paste0(
          "Enrollment in grade ", grade_data$GRADE[i], " was unusually ", direction,
          " than average in ", grade_data$YEAR_LABEL[i], "."
        ))
      }
    }
  }
  
  if (length(outlier_sentences) == 0) {
    outlier_summary <- "No unusual enrollment patterns were detected in any grade."
  } else {
    outlier_summary <- paste(outlier_sentences, collapse = " ")
  }
  
  # --- Summary Paragraph (Executive Summary Style) ---
  summary_sentences <- c("Student enrollment patterns showed notable changes and developments over time.")
  
  if (!is.na(pct_change_recent) && abs(pct_change_recent) >= pct_change_threshold) {
    summary_sentences <- c(summary_sentences, "Overall enrollment shifted noticeably from the previous year.")
  }
  
  if (recent_is_max) {
    summary_sentences <- c(summary_sentences, "The most recent year reached the highest enrollment on record.")
  } else if (recent_is_min) {
    summary_sentences <- c(summary_sentences, "The most recent year had the lowest enrollment on record.")
  }
  
  if (avg_change >= grade_transition_threshold) {
    summary_sentences <- c(summary_sentences, "Enrollment varied notably across grade levels.")
  } else {
    summary_sentences <- c(summary_sentences, "Grade-level enrollment remained fairly steady.")
  }
  
  if (length(outlier_sentences) > 0) {
    summary_sentences <- c(summary_sentences, "Some grades experienced enrollment levels outside the typical range.")
  } else {
    summary_sentences <- c(summary_sentences, "No major unusual enrollment patterns were detected.")
  }
  
  if (length(summary_sentences) == 1) {
    summary_sentences <- c(summary_sentences, "Overall, enrollment remained steady without major shifts.")
  }
  
  Paragraph_Summary <- paste(summary_sentences, collapse = " ")
  
  # --- Detailed Paragraph (Full Detail) ---
  detailed_sentences <- c(
    recent_enrollment_sentence,
    recent_extreme_sentence,
    grade_change_sentence
  )
  
  big_increases <- grade_changes[grade_changes$PctChange >= grade_transition_threshold, ]
  big_decreases <- grade_changes[grade_changes$PctChange <= -grade_transition_threshold, ]
  
  if (nrow(big_increases) > 0) {
    detailed_sentences <- c(detailed_sentences,
                            paste("Grades with the largest increases include:", paste(big_increases$Grade, collapse = ", "), "."))
  }
  if (nrow(big_decreases) > 0) {
    detailed_sentences <- c(detailed_sentences,
                            paste("Grades with the largest decreases include:", paste(big_decreases$Grade, collapse = ", "), "."))
  }
  
  high_avg_grades <- grade_stats$GRADE_NUMERIC[which(grade_stats$MEAN > mean(grade_stats$MEAN))]
  if (length(high_avg_grades) > 0) {
    high_labels <- long_table$GRADE[long_table$GRADE_NUMERIC %in% high_avg_grades]
    detailed_sentences <- c(detailed_sentences, paste("Grades with consistently higher enrollment include:",
                                                      paste(unique(high_labels), collapse = ", "), "."))
  }
  
  # New: Comment on similarity of grade changes
  if (nrow(grade_changes) > 1) {
    abs_changes_sd <- sd(abs(grade_changes$PctChange), na.rm = TRUE)
    if (!is.na(abs_changes_sd)) {
      if (abs_changes_sd < 5) {
        detailed_sentences <- c(detailed_sentences, "Changes in enrollment were fairly consistent across grades.")
      } else {
        detailed_sentences <- c(detailed_sentences, "Some grades experienced larger shifts than others.")
      }
    }
  }
  
  detailed_sentences <- c(detailed_sentences, outlier_summary)
  
  enroll_sd <- sd(total_table$Count, na.rm = TRUE)
  if (!is.na(enroll_sd)) {
    detailed_sentences <- c(detailed_sentences,
                            paste0("Enrollment varied by about ", round(enroll_sd, 0), " students across years."))
  }
  
  Paragraph_Detailed <- paste(detailed_sentences, collapse = " ")
  
  # --- Optional Suggestion Note ---
  note_message <- NULL
  if (length(high_avg_grades) > 0) {
    note_message <- "View Heatmap or Grade_Statistics to examine grades with consistently high enrollment."
  } else if (nrow(big_increases) > 0 || nrow(big_decreases) > 0) {
    note_message <- "View Enrollment_LinePlot or Enrollment_BarPlot to explore grade-level trends."
  }
  
  OUT$Paragraph_Summary <- Paragraph_Summary
  OUT$Paragraph_Detailed <- Paragraph_Detailed
  OUT$Enrollment_Trends <- recent_enrollment_sentence
  OUT$Grade_Level_Changes <- grade_change_sentence
  OUT$Outlier_Summary <- outlier_summary
  OUT$Note <- note_message
  
  invisible(OUT)
}
