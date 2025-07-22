summarize_longitudinal_proficiency <- function(dataset,
                                               use_achievement_levels = TRUE,
                                               content_area_filter = NULL,
                                               grade_filter = NULL,
                                               top_n_proficient = 3,
                                               top_n_advanced = 2) {
  #--- Input checks ---
  if (!is.data.frame(dataset)) stop("'dataset' must be a data frame.")
  if (!"YEAR" %in% names(dataset)) stop("Missing required column: 'YEAR'")
  if (!"CONTENT_AREA" %in% names(dataset)) stop("Missing required column: 'CONTENT_AREA'")
  if (!"GRADE" %in% names(dataset)) stop("Missing required column: 'GRADE'")
  
  level_var <- if (use_achievement_levels) "ACHIEVEMENT_LEVEL" else "PROFICIENCY_LEVEL"
  if (!level_var %in% names(dataset)) {
    stop(paste("Missing expected column:", level_var))
  }
  
  #--- Apply optional filters ---
  df <- dataset
  if (!is.null(content_area_filter)) {
    df <- df[df$CONTENT_AREA %in% content_area_filter, ]
  }
  if (!is.null(grade_filter)) {
    df <- df[df$GRADE %in% grade_filter, ]
  }
  
  #--- Standardize proficiency levels ---
  df$Level <- as.character(df[[level_var]])
  df$Proficiency <- make_proficiency_levels(df$Level)
  
  #--- Define thresholds ---
  level_order <- levels(df$Proficiency)
  proficient_levels <- tail(level_order, top_n_proficient)
  advanced_levels   <- tail(level_order, top_n_advanced)
  
  #--- Add flags ---
  df$Is_Proficient <- df$Proficiency %in% proficient_levels
  df$Is_Advanced   <- df$Proficiency %in% advanced_levels
  
  #--- Aggregate ---
  df$GRADE <- as.character(df$GRADE)
  out <- aggregate(
    cbind(Is_Proficient = df$Is_Proficient, Is_Advanced = df$Is_Advanced),
    by = list(
      YEAR = df$YEAR,
      CONTENT_AREA = df$CONTENT_AREA,
      GRADE = df$GRADE
    ),
    FUN = function(x) mean(x, na.rm = TRUE)
  )
  
  #--- Add total count per group ---
  counts <- aggregate(ID ~ YEAR + CONTENT_AREA + GRADE, data = df, FUN = length)
  names(counts)[names(counts) == "ID"] <- "n_total"
  
  #--- Merge and format ---
  merged <- merge(out, counts, by = c("YEAR", "CONTENT_AREA", "GRADE"))
  merged$pct_proficient <- round(100 * merged$Is_Proficient, 1)
  merged$pct_advanced   <- round(100 * merged$Is_Advanced, 1)
  
  #--- Final columns ---
  final <- merged[, c("YEAR", "CONTENT_AREA", "GRADE", "n_total", "pct_proficient", "pct_advanced")]
  
  return(final)
}
