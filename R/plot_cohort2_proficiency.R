









plot_cohort2_proficiency <- function(dataset, year_range, grade_range, n_proficiencies = NULL, achievement = NULL) {
  # Validate the input
  colnames_lower <- tolower(names(dataset))
  required_variables <- c("grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # If proficiency is not included in the data set, then add it
  optional_variable <- c("proficiency_levels")
  checked_variables <- optional_variable[!optional_variable %in% colnames_lower]
  
  # Identifying the first grade in the grade range
  start_grade = min(grade_range)
  
  # If proficiency is not included in the data set, then add it
  if(length(checked_variable) > 0) {
    if(is.null(n_proficiencies)) {
      n_proficiences <- 2
    }
    if(is.null(achievement)) {
      achievement <- "ACHIEVEMENT_LEVEL"
    }
    dataset <- make_proficiency_levels(dataset = dataset, 
                                      achievement = "ACHIEVEMENT_LEVEL", 
                                      n_proficiences = n_proficiencies)$Data
  }
  
  # Create a new data frame as repository
  DF_NEW = data.frame(
    grade = integer(), 
    year = integer(), 
    np = numeric(), 
    p = numeric(), 
    stringsAsFactors = FALSE
  )
  
  # Determine the length of the inputs
  n_years = length(year_range)
  n_grades = length(grade_range)
  
  # Fill in vectors mixing the grades and years
  years <- rep(year_range, each = n_grades)
  grades <- rep(grade_range, n_years)
  
  # Get character strings for the years
  year_char = paste0((years-1), "_", years)
  
  for(i in 1:length(grades)) {
    TABLE_NEW = table(dataset[dataset$GRADE == grades[i] & dataset$YEAR == year_char[i],
                            "PROFICIENCY_LEVELS"])
    PROP_NEW = round(prop.table(TABLE_NEW), 3) * 100
    TEMP = data.frame(
      grade = grades[i], 
      year = years[i], 
      np = as.numeric(proptable.new["Not Proficient"]), 
      p = as.numeric(proptable.new["Proficient"])
    )
    DF_NEW = rbind(DF_NEW, TEMP)
  }
  
  df_NEW$cohort = DF_NEW$year - DF_NEW$grade + start_grade
  
  PLOT <- ggplot(DF_NEW, aes(x = grade, y = p, group = cohort, 
                             color = as.factor(cohort))) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    scale_color_brewer(palette = "Dark2") +  
    scale_x_continuous(breaks = seq(min(DF_NEW$grade), max(DF_NEW$grade), 1)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    labs(
      x = "Grade",
      y = "Percent Proficient",
      title = "Percent Proficient by Cohort",
      color = paste0("Grade ", start_grade, " in ")  
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  
  OUT <- list()
  OUT$Data <- DF_NEW
  OUT$Plot <- PLOT
  OUT$Caption <- paste()
  OUT$
  
  # Invisibly return the output
  return(invisible(OUT))
}