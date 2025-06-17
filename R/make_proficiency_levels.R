#' Create Proficiency Levels from Ordered Achievement Factor
#'
#' Converts an ordered factor achievement variable into a binary proficiency variable by
#' grouping the top `n_proficiencies` achievement levels as "Proficient" and the rest as "Not Proficient".
#'
#' @title Make Proficiency Levels
#' 
#' @param dataset A data frame containing the achievement variable.
#' @param achievement A string specifying the column name of the achievement factor variable.
#'        The variable must be a factor with ordered levels from lowest to highest.
#' @param n_proficiencies An integer (≥1) specifying how many of the highest achievement levels
#'        to classify as "Proficient".
#' @param print_plot Logical (default: \code{FALSE}). If \code{TRUE}, prints a bar plot of proficiency level distribution.
#' @param proficiency_labels A character vector of length 2 specifying the labels for
#'        proficiency levels, in order: c("Not Proficient", "Proficient").
#'        Default is c("Not Proficient", "Proficient").
#'
#' @return A list containing:
#' \describe{
#'   \item{Data}{The input dataset with an added factor variable \code{PROFICIENCY_LEVELS}.}
#'   \item{Table}{A summary table showing percentages of each proficiency level.}
#'   \item{Caption}{A descriptive caption string about the proficiency grouping.}
#'   \item{Plot}{A ggplot2 barplot object of the proficiency distribution.}
#' }
#'
#' @details
#' The function verifies inputs, orders the achievement factor if unordered,
#' and classifies observations into proficiency based on the top `n_proficiencies` achievement levels.
#'
#' @importFrom stats aggregate setNames
#' @import ggplot2
#' @export
#' 
#' @examples
#' make_proficiency_levels(dataset = math, achievement = "ACHIEVEMENT_LEVEL", n_proficiencies = 2)
#' 

make_proficiency_levels = function(dataset, achievement, n_proficiencies, print_plot = FALSE,
                                   proficiency_labels = c("Not Proficient", "Proficient")) {
  # Validate inputs
  if(!is.numeric(n_proficiencies) || floor(n_proficiencies) != n_proficiencies) {
    stop("The number of proficiency levels must be a whole number.")
  }
  if (n_proficiencies < 1) {
    stop("The number of proficiency levels must be at least 1.")
  }
  
  # Validate proficiency_labels length
  if(length(proficiency_labels) != 2) {
    stop("proficiency_labels must be a character vector of length 2.")
  }
  
  # Validate column names
  colnames_lower = tolower(names(dataset))
  if (!(tolower(achievement) %in% colnames_lower)) {
    stop(paste("Achievement variable", achievement, "not found in dataset."))
  }
  
  # Map actual column name (in case of case mismatch)
  name_map = setNames(names(dataset), colnames_lower)
  achievement_col = name_map[[tolower(achievement)]]
  
  # Initialize output
  OUT = list()
  
  # Convert to data frame if not already
  OUT$Data = as.data.frame(dataset)
  
  # Prepare variables
  ACHIEVEMENTS = OUT$Data[[achievement_col]]
  
  # Check that achievement levels are factors
  if (!is.factor(ACHIEVEMENTS)) {
    stop("The 'achievement_level' column must be a factor with ordered levels from lowest to highest.")
  }
  
  # Ensure the factor is ordered
  if (!is.ordered(ACHIEVEMENTS)) {
    ACHIEVEMENTS = factor(ACHIEVEMENTS, levels = levels(ACHIEVEMENTS), ordered = TRUE)
  }
  
  # Identify the levels of achievements
  LEVELS = levels(ACHIEVEMENTS)
  
  # Number of achievement levels
  N_LEVELS = length(LEVELS)
  
  # Error Detection
  if(N_LEVELS < n_proficiencies) {
    stop("The number of proficiency levels cannot exceed the number of achievement levels.")
  }
  
  # Binary conversion to proficiency with custom labels
  TOP_LEVELS = LEVELS[(N_LEVELS - n_proficiencies + 1):N_LEVELS]
  BINARY = ifelse(is.na(ACHIEVEMENTS), NA, 
                  ACHIEVEMENTS %in% TOP_LEVELS)
  PROFICIENCY = factor(BINARY, levels = c(FALSE, TRUE), labels = proficiency_labels)
  
  # Append to the data set
  OUT$Data$PROFICIENCY_LEVELS = PROFICIENCY
  
  # Make summary tables
  TABLE1 = round(prop.table(table(PROFICIENCY)), 3) * 100
  TABLE2 = sapply(TABLE1, function(x) {sprintf("%.1f%%", x)})
  
  # Output the table
  OUT$Table = t(TABLE2)
  
  # Make a caption
  caption_text <- paste("Distribution of proficiency status using top", n_proficiencies, "achievement level(s)")
  OUT$Caption = caption_text
  
  # Prepare plot data
  PLOT_df <- data.frame(
    PROFICIENCY = names(TABLE1),
    PERCENT = as.numeric(TABLE1)
  )
  
  # Make a barplot
  PLOT1 = ggplot(PLOT_df, aes(x = PROFICIENCY, y = PERCENT, fill = PROFICIENCY)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("lightgray", "steelblue")) +
    labs(title = "Proficiency Distribution", y = "Percent", x = NULL) +
    theme_minimal()
  OUT$Plot = PLOT1
  
  # Print the barplot
  if(print_plot) print(PLOT1)
  
  # Return the output invisibly
  return(invisible(OUT))
}
