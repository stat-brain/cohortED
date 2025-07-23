#' Plot Percent Proficient by Cohort
#'
#' This function calculates and visualizes the percentage of students who are
#' proficient, grouped by grade and year, and displays longitudinal trends
#' by synthetic cohorts (groups of students assumed to progress together).
#' These synthetic cohorts are constructed by aligning grade levels over years,
#' and may not represent the same individuals over time.
#'
#' @param dataset A data frame containing at least `grade` and `year` columns.
#' @param year_range A numeric vector of years to include (e.g., 2020:2024).
#' @param grade_range A numeric vector of grade levels to include (e.g., 3:5).
#' @param n_proficiencies Optional. Number of proficiency levels to compute 
#'        if `proficiency_levels` column is missing. Defaults to 2.
#' @param achievement Optional. Name of the achievement level column used to 
#'        derive proficiency levels. Defaults to `"ACHIEVEMENT_LEVEL"`.
#'
#' @return An invisible list with the following components:
#' \describe{
#'   \item{Data}{A tidy data frame containing percent proficient and not proficient by grade and year.}
#'   \item{Table}{A grade-by-year table of percent proficient values.}
#'   \item{Plot}{A ggplot2 object visualizing cohort trends across grades.}
#'   \item{Caption}{A character string describing the plot, including a note that cohorts are synthetic.}
#' }
#'
#' @note This function constructs synthetic (or pseudo) cohorts by aligning
#' grades over sequential years. These do not reflect actual student-level 
#' longitudinal data and should be interpreted accordingly.
#'
#' @importFrom stats aggregate setNames
#' @import ggplot2
#' @export
#' 
#' @examples
#' plot_cohort2_proficiency(dataset = math, year_range = 2020:2024, grade_range = 3:5)
#' 


plot_cohort2_proficiency <- function(dataset, year_range, grade_range, 
                                     n_proficiencies = NULL, achievement = NULL) {
  
  # Validate required columns
  colnames_lower <- tolower(names(dataset))
  required_variables <- c("grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if (length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Flexible name mapping
  name_map <- setNames(names(dataset), tolower(names(dataset)))
  grade_col <- name_map["grade"]
  year_col  <- name_map["year"]
  
  # Add proficiency levels if needed
  if (!"proficiency_levels" %in% colnames_lower) {
    if (is.null(n_proficiencies)) n_proficiencies <- 2
    if (is.null(achievement)) achievement <- "ACHIEVEMENT_LEVEL"
    
    dataset <- make_proficiency_levels(
      dataset = dataset, 
      achievement = achievement, 
      n_proficiencies = n_proficiencies
    )$Data
  }
  
  start_grade <- min(grade_range)
  
  # Prepare year/grade combinations
  n_years <- length(year_range)
  n_grades <- length(grade_range)
  grades <- rep(grade_range, times = n_years)
  start_years <- rep(year_range, each = n_grades)  # These are academic year *start* years
  year_labels <- paste0(start_years, "_", start_years + 1)
  
  # Initialize results
  DF_NEW <- data.frame(grade = integer(), 
                       year = integer(), 
                       np = numeric(), 
                       p = numeric())
  
  for (i in seq_along(grades)) {
    subset_vec <- dataset[[grade_col]] == grades[i] & dataset[[year_col]] == year_labels[i]
    tbl <- table(dataset[subset_vec, "PROFICIENCY_LEVELS"])
    prop_tbl <- round(prop.table(tbl), 3) * 100
    
    np_val <- if ("Not Proficient" %in% names(prop_tbl)) prop_tbl["Not Proficient"] else 0
    p_val  <- if ("Proficient" %in% names(prop_tbl)) prop_tbl["Proficient"] else 0
    
    TEMP <- data.frame(
      grade = grades[i],
      year = start_years[i],  # Match YEAR_NUM convention (end year of academic year)
      np = as.numeric(np_val),
      p  = as.numeric(p_val)
    )
    
    DF_NEW <- rbind(DF_NEW, TEMP)
  }
  
  # Add cohort and academic year label
  DF_NEW$cohort <- DF_NEW$year - DF_NEW$grade + start_grade
  DF_NEW$Academic_Year <- to_academic_year(DF_NEW$year)
  
  # Rename columns
  colnames(DF_NEW)[1:5] <- c("Grade", "Year", "% Not Proficient", "% Proficient", "Cohort")
  
  # Drop incomplete rows
  DF_NEW <- subset(DF_NEW, !is.na(`% Proficient`))
  DF_NEW <- subset(DF_NEW, is.finite(`% Proficient`) & `% Proficient` >= 0 & `% Proficient` <= 100)
  
  
  # Plot
  PLOT <- ggplot(DF_NEW, aes(x = Grade, y = `% Proficient`, group = Cohort, color = as.factor(Cohort))) +
    geom_point(size = 3) +
    geom_line(linewidth = 1) +
    scale_color_viridis_d(option = "C", end = 0.95) +
    scale_x_continuous(breaks = seq(min(DF_NEW$Grade), max(DF_NEW$Grade), 1)) +
    labs(
      x = "Grade",
      y = "Percent Proficient",
      title = "Percent Proficient by Cohort",
      color = paste0("Grade ", start_grade, " Cohort")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
  
  # Table with academic year labels
  TABLE <- xtabs(`% Proficient` ~ Grade + Academic_Year, data = DF_NEW)
  
  # Return results
  OUT = list(
    Data = DF_NEW,
    Table = TABLE,
    Plot = PLOT,
    Caption = paste(
      "Percent proficient by grade and synthetic cohort for academic years",
      to_academic_year(min(year_range) + 1), "to", to_academic_year(max(year_range) + 1)
    ),
    Note = "Note: Cohorts are synthetic and may not represent the same students across years."
  )
  
  return(invisible(OUT))
}
