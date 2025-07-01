#' Plot an Alluvial Diagram of Student Mobility
#'
#' @title Plot Alluvial Mobility
#' @description Creates an alluvial diagram that illustrates student mobility between years, segmented by gender and ethnicity. If `MOBILITY_STATUS` is not already present in the dataset, it will be computed using the provided grade and year.
#'
#' @param dataset A data frame that includes student grade level, academic year, gender, and ethnicity.
#' @param current_year The academic year to evaluate (e.g., "2020_2021").
#' @param current_grade The grade level to evaluate (e.g., 4 or "4").
#' @param print_plot Logical; if TRUE (default), the alluvial plot is printed.
#'
#' @return A list containing:
#' \describe{
#'   \item{Data}{The updated dataset used for plotting}
#'   \item{Data_Table}{A contingency table of Mobility x Gender x Ethnicity}
#'   \item{Table_by_Ethnicity}{Proportion table by ethnicity}
#'   \item{Table_by_Gender}{Proportion table by gender}
#'   \item{Caption}{Text summary of transition between grades}
#'   \item{Plot}{The generated ggplot2 object}
#' }
#'
#' @import ggplot2
#' @importFrom ggalluvial geom_alluvium geom_stratum StatStratum
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' plot_alluvial_mobility(dataset = math, current_year = "2020_2021", current_grade = 4)
#' 

plot_alluvial_mobility <- function(dataset, current_year, current_grade, print_plot = TRUE) {
  # Normalize current grade input
  numeric_grade <- .normalize_grade(current_grade)
  
  # Standardize column names to lowercase
  names(dataset) <- tolower(names(dataset))
  
  # Create mobility_status if missing
  if (!"mobility_status" %in% names(dataset)) {
    dataset <- make_mobility(dataset, current_year, current_grade, print_plot = FALSE)$Data
    names(dataset) <- tolower(names(dataset))  # Re-standardize after update
  }
  
  # Combining Gender
  if (!"gender" %in% names(dataset)) {
    if ("gender.x" %in% names(dataset) && "gender.y" %in% names(dataset)) {
      # Convert factors to characters first
      gx <- as.character(dataset$gender.x)
      gy <- as.character(dataset$gender.y)
      
      # Combine by choosing gender.y if not NA, else gender.x
      combined <- ifelse(!is.na(gy), gy, gx)
      
      # Convert back to factor with all unique levels from both columns
      levels_gender <- levels(dataset$gender.y)
      dataset$gender <- factor(combined, levels = levels_gender)
    } else if ("gender.x" %in% names(dataset)) {
      dataset$gender <- factor(as.character(dataset$gender.x))
    } else if ("gender.y" %in% names(dataset)) {
      dataset$gender <- factor(as.character(dataset$gender.y))
    }
  }
  
  # Combining Ethnicity
  if (!"ethnicity" %in% names(dataset)) {
    if ("ethnicity.x" %in% names(dataset) && "ethnicity.y" %in% names(dataset)) {
      ex <- as.character(dataset$ethnicity.x)
      ey <- as.character(dataset$ethnicity.y)
      combined <- ifelse(!is.na(ey), ey, ex)
      levels_ethnicity <- levels(dataset$ethnicity.y)
      dataset$ethnicity <- factor(combined, levels = levels_ethnicity)
    } else if ("ethnicity.x" %in% names(dataset)) {
      dataset$ethnicity <- factor(as.character(dataset$ethnicity.x))
    } else if ("ethnicity.y" %in% names(dataset)) {
      dataset$ethnicity <- factor(as.character(dataset$ethnicity.y))
    }
  }
  
  # Convert variables to factors
  dataset$mobility_status <- as.factor(dataset$mobility_status)
  
  # Create year label strings
  last_year_grade <- numeric_grade - 1
  last_year_grade <- ifelse(last_year_grade == 0, "K",
                            ifelse(last_year_grade == -1, "PreK", last_year_grade))
  last_year_text <- paste0(as.numeric(substr(current_year, 1, 4)) - 1, "-",
                           as.numeric(substr(current_year, 1, 4)))
  current_year_text <- paste0(as.numeric(substr(current_year, 1, 4)), "-",
                              as.numeric(substr(current_year, 1, 4)) + 1)
  
  # Construct alluvial plot data
  tab <- table(dataset$mobility_status, dataset$gender, dataset$ethnicity)
  df <- as.data.frame(tab)
  names(df) <- c("Mobility", "Gender", "Ethnicity", "Frequency")
  
  # Filter out zero-frequency rows
  df <- subset(df, Frequency > 0)
  
  # If there are not enough levels for a plot, skip
  if (length(unique(df$Mobility)) < 2 ||
      length(unique(df$Gender)) < 2 ||
      length(unique(df$Ethnicity)) < 2) {
    warning("Not enough levels in one or more dimensions to build an alluvial plot.")
    return(invisible(NULL))
  }
  
  # Ensure variables are factors
  df$Mobility <- factor(df$Mobility)
  df$Gender <- factor(df$Gender, levels = levels_gender)
  df$Ethnicity <- factor(df$Ethnicity, levels = levels_ethnicity)
  
  # Build alluvial plot
  plot <- ggplot(df,
                 aes(axis1 = Mobility, axis2 = Gender, axis3 = Ethnicity, y = Frequency)) +
    ggalluvial::geom_alluvium(aes(fill = Mobility)) +
    ggalluvial::geom_stratum() +
    ggplot2::geom_text(stat = ggalluvial::StatStratum, aes(label = after_stat(stratum)), size = 3) +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste("Grade", current_grade, "in", current_year_text))
  
  # Create proportion tables
  by_ethnicity <- round(prop.table(table(dataset$mobility_status, dataset$ethnicity), margin = 2), 3) * 100
  by_gender <- round(prop.table(table(dataset$mobility_status, dataset$gender), margin = 2), 3) * 100
  
  by_ethnicity <- apply(by_ethnicity, c(1, 2), function(x) sprintf("%.1f%%", x))
  by_gender <- apply(by_gender, c(1, 2), function(x) sprintf("%.1f%%", x))
  
  # Return results
  out <- list(
    Data = dataset,
    Data_Table = tab,
    Table_by_Ethnicity = by_ethnicity,
    Table_by_Gender = by_gender,
    Caption = paste("Demographics of Students from Grade", last_year_grade, "in", last_year_text,
                    "to Grade", current_grade, "in", current_year_text),
    Plot = plot
  )
  
  if (print_plot) print(plot)
  
  return(invisible(out))
}


