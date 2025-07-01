

analyze_within_mobility <- function(dataset, level, current_year) {
  to_academic_year <- function(year_numeric) {
    paste0(year_numeric, "-", year_numeric + 1)
  }
  
  OUT <- list()
  
  names(dataset) <- tolower(names(dataset))
  level <- tolower(level)
  
  required_vars <- c("id", "year", level, "grade")
  missing_vars <- setdiff(required_vars, names(dataset))
  if (length(missing_vars) > 0) {
    stop("Dataset is missing required columns: ", paste(missing_vars, collapse = ", "))
  }
  
  if (is.character(current_year)) {
    current_year <- as.numeric(substr(current_year, 1, 4))
  }
  previous_year <- current_year - 1
  
  # Filter previous and current data
  data_prev <- dataset[dataset$year == previous_year, c("id", level, "grade")]
  data_curr <- dataset[dataset$year == current_year,  c("id", level, "grade")]
  colnames(data_prev) <- c("id", "Previously", "Grade_prev")
  colnames(data_curr) <- c("id", "Currently", "Grade_curr")
  
  # Merge on student ID
  merged <- merge(data_prev, data_curr, by = "id", all = TRUE)
  
  # Initial mobility classification
  merged$Mobility <- ifelse(!is.na(merged$Previously) & !is.na(merged$Currently),
                            ifelse(merged$Previously == merged$Currently, "Stay", "Move"),
                            ifelse(is.na(merged$Previously), "Join", "Leave"))
  
  # --- Structural Progression Detection ---
  valid_rows <- !is.na(merged$Grade_prev) & !is.na(merged$Grade_curr)
  grade_pairs <- unique(merged[valid_rows, c("Grade_prev", "Grade_curr")])
  
  for (i in seq_len(nrow(grade_pairs))) {
    gp <- grade_pairs[i, ]
    rows <- merged$Grade_prev == gp$Grade_prev & merged$Grade_curr == gp$Grade_curr
    if (all(merged$Mobility[rows] %in% c("Move", "Leave"))) {
      merged$Mobility[rows & merged$Mobility %in% c("Move", "Leave")] <- "Progression"
    }
  }
  
  OUT$Data <- merged
  
  # --- Summary Tables ---
  if (all(is.na(merged$Mobility))) {
    warning("All mobility values are NA or missing. Check data inputs.")
    summary_all <- data.frame(Mobility = character(0), Count = integer(0), Percent = numeric(0))
  } else {
    summary_all <- as.data.frame(table(merged$Mobility), stringsAsFactors = FALSE)
    colnames(summary_all) <- c("Mobility", "Count")
    summary_all$Percent <- round(100 * summary_all$Count / sum(summary_all$Count), 1)
  }
  
  OUT$Summary <- summary_all
  
  summary_filtered <- summary_all[summary_all$Mobility != "Progression", ]
  if (nrow(summary_filtered) > 0) {
    total_filtered <- sum(summary_filtered$Count)
    summary_filtered$Percent <- round(100 * summary_filtered$Count / total_filtered, 1)
  }
  OUT$Summary_Excluding_Progression <- summary_filtered
  
  # --- Level Summary ---
  get_count <- function(df, group_col, name) {
    if (nrow(df) == 0) return(data.frame(Level = character(0), Count = integer(0)))
    tbl <- table(df[[group_col]])
    return(data.frame(Level = names(tbl), Count = as.integer(tbl), stringsAsFactors = FALSE))
  }
  
  join_df <- get_count(merged[merged$Mobility == "Join", ], "Currently", "Join")
  stay_df <- get_count(merged[merged$Mobility == "Stay", ], "Currently", "Stay")
  movein_df <- get_count(merged[merged$Mobility == "Move", ], "Currently", "MoveIn")
  leave_df <- get_count(merged[merged$Mobility == "Leave", ], "Previously", "Leave")
  moveout_df <- get_count(merged[merged$Mobility == "Move", ], "Previously", "MoveOut")
  prog_in_df <- get_count(merged[merged$Mobility == "Progression", ], "Currently", "ProgressedIn")
  prog_out_df <- get_count(merged[merged$Mobility == "Progression", ], "Previously", "ProgressedOut")
  
  # Combine all counts
  all_levels <- unique(c(join_df$Level, stay_df$Level, movein_df$Level,
                         leave_df$Level, moveout_df$Level, prog_in_df$Level, prog_out_df$Level))
  
  level_summary <- data.frame(Level = all_levels, stringsAsFactors = FALSE)
  
  merge_count <- function(df, col_name) {
    merge(level_summary, df, by = "Level", all.x = TRUE)[, col_name]
  }
  
  level_summary$Join           <- merge_count(join_df, "Count")
  level_summary$Stay           <- merge_count(stay_df, "Count")
  level_summary$MoveIn         <- merge_count(movein_df, "Count")
  level_summary$ProgressedIn   <- merge_count(prog_in_df, "Count")
  level_summary$Leave          <- merge_count(leave_df, "Count")
  level_summary$MoveOut        <- merge_count(moveout_df, "Count")
  level_summary$ProgressedOut  <- merge_count(prog_out_df, "Count")
  
  level_summary[is.na(level_summary)] <- 0
  
  # Totals and percentages
  level_summary$TotalIn   <- with(level_summary, Join + Stay + MoveIn + ProgressedIn)
  level_summary$TotalOut  <- with(level_summary, Leave + MoveOut + ProgressedOut)
  level_summary$NetChange <- with(level_summary, TotalIn - TotalOut)
  
  level_summary$PercentJoin          <- round(100 * level_summary$Join / level_summary$TotalIn, 1)
  level_summary$PercentMoveIn        <- round(100 * level_summary$MoveIn / level_summary$TotalIn, 1)
  level_summary$PercentProgressedIn  <- round(100 * level_summary$ProgressedIn / level_summary$TotalIn, 1)
  level_summary$PercentLeave         <- round(100 * level_summary$Leave / level_summary$TotalOut, 1)
  level_summary$PercentMoveOut       <- round(100 * level_summary$MoveOut / level_summary$TotalOut, 1)
  level_summary$PercentProgressedOut <- round(100 * level_summary$ProgressedOut / level_summary$TotalOut, 1)
  
  level_summary[is.na(level_summary)] <- 0
  
  OUT$By_Level <- level_summary
  OUT$Academic_Year <- to_academic_year(previous_year)
  
  return(invisible(OUT))
}

