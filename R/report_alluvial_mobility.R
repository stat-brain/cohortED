#' @title Report Summary of Alluvial Mobility Patterns
#'
#' @description
#' Generates a plain-language summary based on the output from `plot_alluvial_mobility()`,
#' including gender, ethnicity, and White vs. non-White mobility comparisons. Also provides
#' context and reporting notes based on observed differences.
#'
#' @param alluvial_output A list returned from `plot_alluvial_mobility()`.
#' @param gap_threshold Numeric. Minimum percent difference to flag a group-level difference (default = 5).
#'
#' @return A named list with summary paragraphs, subgroup comparisons, context labels, and interpretation notes.
#' @export
#' 

report_alluvial_mobility <- function(alluvial_output, gap_threshold = 5) {
  gender_tab <- alluvial_output$Table_by_Gender
  eth_tab <- alluvial_output$Table_by_Ethnicity
  caption <- alluvial_output$Caption
  
  # Extract grade and year from caption
  matches <- regmatches(caption, regexec("Grade (\\w+) in (\\d{4}[-\u2013]\\d{4}) to Grade (\\w+) in (\\d{4}[-\u2013]\\d{4})", caption))[[1]]
  prior_grade <- matches[2]
  prior_year <- matches[3]
  current_grade <- matches[4]
  current_year <- matches[5]
  
  # Helper: calculate max spread across mobility categories for each group
  max_diff_by_group <- function(tbl) {
    apply(tbl, 2, function(col) {
      values <- suppressWarnings(as.numeric(sub("%", "", col)))
      diff <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
      round(diff, 1)
    })
  }
  
  # Gender and ethnicity differences
  gender_diff <- max_diff_by_group(gender_tab)
  eth_diff <- max_diff_by_group(eth_tab)
  max_gender_gap <- max(gender_diff, na.rm = TRUE)
  max_eth_gap <- max(eth_diff, na.rm = TRUE)
  gender_note <- names(which.max(gender_diff))
  eth_note <- names(which.max(eth_diff))
  
  # Executive summary
  summary <- paste(
    "Mobility from Grade", prior_grade, "(", prior_year, ") to Grade", current_grade, 
    "(", current_year, ") was analyzed by gender and ethnicity.",
    "The alluvial diagram highlights how those patterns intersect with key demographic groups.",
    if (max_gender_gap >= gap_threshold) {
      paste("Differences by gender were most pronounced among", gender_note, "students.")
    } else {
      "Overall, mobility rates were fairly consistent across gender groups."
    },
    if (max_eth_gap >= gap_threshold) {
      paste("Larger differences emerged by ethnicity, particularly for", eth_note, "students.")
    } else {
      "No major differences in mobility were observed across ethnic groups."
    }
  )
  
  
  # Extract top gender and ethnicity columns
  gender_values <- suppressWarnings(as.numeric(sub("%", "", gender_tab[, gender_note])))
  names(gender_values) <- rownames(gender_tab)
  eth_values <- suppressWarnings(as.numeric(sub("%", "", eth_tab[, eth_note])))
  names(eth_values) <- rownames(eth_tab)
  
  # Get the highest and lowest mobility category for each
  gender_max_cat <- names(gender_values)[which.max(gender_values)]
  gender_max_val <- round(max(gender_values, na.rm = TRUE), 1)
  gender_min_cat <- names(gender_values)[which.min(gender_values)]
  gender_min_val <- round(min(gender_values, na.rm = TRUE), 1)
  
  eth_max_cat <- names(eth_values)[which.max(eth_values)]
  eth_max_val <- round(max(eth_values, na.rm = TRUE), 1)
  eth_min_cat <- names(eth_values)[which.min(eth_values)]
  eth_min_val <- round(min(eth_values, na.rm = TRUE), 1)
  
  # Gender:Ethnicity interaction highlight
  alluvial_tab <- as.data.frame(alluvial_output$Data_Table)
  names(alluvial_tab) <- c("Mobility", "Gender", "Ethnicity", "Frequency")
  
  # Total counts per (Gender:Ethnicity) subgroup
  subgroup_totals <- aggregate(Frequency ~ Gender + Ethnicity, data = alluvial_tab, sum)
  
  # Join totals and calculate percent per subgroup
  merged <- merge(alluvial_tab, subgroup_totals, by = c("Gender", "Ethnicity"), suffixes = c("", "_Total"))
  merged$Percent <- 100 * merged$Frequency / merged$Frequency_Total
  
  # Find subgroup with highest mobility rate in Join/Leave
  interaction_rows <- merged[merged$Mobility %in% c("Join", "Leave"), ]
  max_row <- interaction_rows[which.max(interaction_rows$Percent), ]
  
  detailed <- paste(
    "Across gender groups, the largest observed gap in mobility rates was", max_gender_gap, "percentage points.",
    if (max_gender_gap >= gap_threshold) {
      paste0(
        gender_note, " students were most likely to be categorized as '", gender_max_cat, "' (", gender_max_val, "%), ",
        "and least likely as '", gender_min_cat, "' (", gender_min_val, "%), indicating notable variation."
      )
    },
    "Among ethnic groups, the largest gap was", max_eth_gap, "percentage points.",
    if (max_eth_gap >= gap_threshold) {
      paste0(
        eth_note, " students had the highest share in the '", eth_max_cat, "' category (", eth_max_val, "%), ",
        "and the lowest in '", eth_min_cat, "' (", eth_min_val, "%), reflecting meaningful variation across groups."
      )
    }
  )
  
  # Append intersectional insight
  detailed <- paste(
    detailed,
    paste(
      "When examining intersections of gender and ethnicity, the highest observed mobility rate was among",
      paste(max_row$Gender, max_row$Ethnicity, "students"), "who were classified as",
      paste0("'", max_row$Mobility, "' (", round(max_row$Percent, 1), "%)."),
      "This suggests that certain demographic subgroups may experience elevated levels of mobility."
    )
  )
  
  gender_summary <- if (max_gender_gap >= gap_threshold) {
    paste(
      "The greatest variation in mobility rates by gender was observed among", gender_note, "students,",
      "with a spread of", max_gender_gap, "percentage points across mobility categories."
    )
  } else {
    "Mobility rates were relatively consistent across gender groups, with only minor differences observed."
  }
  
  ethnicity_summary <- if (max_eth_gap >= gap_threshold) {
    paste(
      "Mobility rates varied across ethnic groups, with", eth_note, "students showing the greatest differences across categories.",
      "This suggests that ethnicity may be associated with meaningful variation in student movement."
    )
  } else {
    "Mobility rates were generally similar across ethnic groups, with no group standing out as markedly different."
  }
  
  # Convert % strings to numeric
  eth_df <- as.data.frame(alluvial_output$Table_by_Ethnicity)
  eth_df[] <- lapply(eth_df, function(col) suppressWarnings(as.numeric(sub("%", "", col))))
  
  # Identify columns
  white_cols <- which(colnames(eth_df) == "White")
  nonwhite_cols <- setdiff(seq_along(eth_df), white_cols)
  
  if (length(nonwhite_cols) > 0 && length(white_cols) > 0) {
    white_avg <- rowMeans(eth_df[, white_cols, drop = FALSE], na.rm = TRUE)
    nonwhite_avg <- rowMeans(eth_df[, nonwhite_cols, drop = FALSE], na.rm = TRUE)
    mobility_categories <- rownames(eth_df)
    
    # Narrative construction
    minority_lines <- paste(
      "When comparing mobility patterns, non-White students were, on average:",
      paste(mobility_categories, sprintf("%.1f%%", nonwhite_avg), collapse = ", "), ".",
      "In contrast, White students were:",
      paste(mobility_categories, sprintf("%.1f%%", white_avg), collapse = ", "), ".",
      "These differences may reflect broader trends in how student mobility intersects with race and ethnicity."
    )
    
    # Optional flag if Stay difference is large
    stay_diff <- abs(nonwhite_avg["Stay"] - white_avg["Stay"])
    if (!is.na(stay_diff) && stay_diff >= gap_threshold) {
      minority_lines <- paste(
        minority_lines,
        paste(
          "The difference in 'Stay' rates alone was", sprintf("%.1f%%", stay_diff),
          "which may warrant further exploration of systemic or contextual factors."
        )
      )
    }
    
  } else {
    minority_lines <- "White/non-White comparisons could not be calculated due to missing or collapsed ethnicity labels."
  }
  
  
  # Start with a base message
  note_parts <- c(
    "The alluvial diagram offers a visual overview of mobility patterns by gender and ethnicity."
  )
  
  # Add gender-specific recommendation
  if (max_gender_gap >= gap_threshold) {
    note_parts <- c(note_parts, "Include the `Table_by_Gender` output in your report to highlight meaningful differences in mobility across gender groups.")
  }
  
  # Add ethnicity-specific recommendation
  if (max_eth_gap >= gap_threshold) {
    note_parts <- c(note_parts, "Include the `Table_by_Ethnicity` output to illustrate substantial variation across ethnic groups.")
  }
  
  # Add intersectional recommendation if relevant subgroup has high mobility
  if (!is.null(max_row) && max_row$Percent >= gap_threshold) {
    note_parts <- c(note_parts, paste(
      "Because", max_row$Gender, max_row$Ethnicity, "students had a notably high rate of",
      tolower(max_row$Mobility), "status (", round(max_row$Percent, 1), "%),",
      "consider including the full `Data_Table` to support subgroup-level discussion."
    ))
  }
  
  # Fallback suggestion
  if (length(note_parts) == 1) {
    note_parts <- c(note_parts, "Use the gender and ethnicity tables to explore minor differences that may still be of interest.")
  }
  
  # Combine into final note
  note <- paste(note_parts, collapse = " ")
  
  # Build output list
  OUT <- list()
  OUT$Paragraph_Summary <- summary
  OUT$Paragraph_Detailed <- detailed
  OUT$Mobility_By_Gender <- gender_summary
  OUT$Mobility_By_Ethnicity <- ethnicity_summary
  OUT$Mobility_By_Minority <- minority_lines
  OUT$Caption <- caption
  OUT$Prior_Grade <- prior_grade
  OUT$Prior_Year <- prior_year
  OUT$Current_Grade <- current_grade
  OUT$Current_Year <- current_year
  OUT$Note <- note
  
  invisible(OUT)
}
