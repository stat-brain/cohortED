#' Create Ordered Achievement Levels Based on Score Cutoffs
#'
#' This function categorizes numeric scores into ordered achievement levels
#' based on specified cutoff points. The achievement levels are returned as
#' an ordered factor to preserve their natural ranking.
#'
#' @title Make Achievement Levels
#' 
#' @param dataset A data frame containing the score variable.
#' @param score A string specifying the name of the numeric score variable
#'   in the dataset to be categorized.
#' @param cuts A numeric vector of cutoff points that includes the minimum and
#'   maximum possible scores as endpoints. Must be sorted or will be sorted
#'   internally. The length of \code{cuts} should be one more than the desired
#'   number of achievement levels.
#' @param achievement_labels An optional character or factor vector specifying
#'   labels for the achievement levels in increasing order. If \code{NULL},
#'   numeric labels (1, 2, ...) will be used. The length must match the number
#'   of intervals defined by \code{cuts}.
#' @param print_plot Logical (default: \code{FALSE}). If \code{TRUE}, a bar plot showing the distribution
#'   of achievement levels will be printed.
#'
#' @return A list containing:
#' \describe{
#'   \item{Data}{Original dataset with a new column \code{ACHIEVEMENT_LEVELS} that is
#'   an ordered factor representing achievement categories.}
#'   \item{Table}{A table of percentage distribution of achievement levels.}
#'   \item{Caption}{A descriptive caption of the achievement level distribution.}
#'   \item{Barplot}{A \code{ggplot2} barplot object of the achievement level distribution.}
#' }
#'
#' @details
#' The function uses \code{cut()} to categorize scores into ordered achievement
#' levels. It explicitly converts the result to an ordered factor to preserve
#' the ranking of levels, which is important for downstream analyses or plotting
#' where order matters.
#'
#' The cutoff points (\code{cuts}) must cover the full score range, including
#' minimum and maximum values. Labels should be provided in increasing order
#' to correctly represent achievement progression.
#' 
#' @importFrom stats aggregate setNames
#' @import ggplot2
#' @export
#' 
#' @examples
#' make_achievement_levels(dataset = math, score = "SCALE_SCORE", cuts = c(0, 500, 700, 1000),
#'                         achievement_labels = c("Unsatisfactory", "Satisfactory", "Advanced"))
#' 

make_achievement_levels = function(dataset, score, cuts, achievement_labels = NULL, print_plot = FALSE) {
  ### cuts needs to include the minimum possible score and maximum possible score as endpoints
  ### achievement_labels should be provided in increasing order
  # Ensure that the cutoff points are numerically ordered
  cuts = sort(cuts)
  
  # Determine the number of categories
  n_levels = length(cuts) - 1
  
  # If labels were not provided, then describe them
  if(is.null(achievement_labels)) {
    achievement_labels = 1:n_levels
  } else {
    # Check to make sure that the number labels matches the number of categories
    if(n_levels != length(achievement_labels)) {
      stop("The number of labels must be the same as the number of proficiency levels.")
    }
  }
  
  # Validate column names
  colnames_lower = tolower(names(dataset))
  if (!(tolower(score) %in% colnames_lower)) {
    stop(paste("Score variable", score, "not found in dataset."))
  }
  
  # Map actual column name (in case of case mismatch)
  name_map = setNames(names(dataset), colnames_lower)
  score_col = name_map[[tolower(score)]]
  
  # Initialize Output
  OUT = list()
  
  # Convert to data frame if not already
  OUT$Data = as.data.frame(dataset, stringsAsFactors = FALSE)
  
  # Convert the labels to ordered factors
  achievement_labels = factor(achievement_labels, levels = achievement_labels, ordered = TRUE)
  
  # Perform the categorization
  ACHIEVEMENT = cut(x = OUT$Data[[score_col]], 
                    breaks = cuts, 
                    labels = achievement_labels, 
                    right = FALSE,
                    include.lowest = TRUE)
  
  # Explicitly convert ACHIEVEMENT to an ordered factor
  ACHIEVEMENT = factor(ACHIEVEMENT, levels = achievement_labels, ordered = TRUE)
  
  # Append to the data set
  OUT$Data$ACHIEVEMENT_LEVELS = ACHIEVEMENT
  
  # Make the summary tables
  TABLE1 = round(prop.table(table(ACHIEVEMENT)), 3) * 100
  TABLE2 = sapply(TABLE1, function(x) {sprintf("%.1f%%", x)})
  
  # Add to the output
  OUT$Table = t(TABLE2)
  
  # Make a caption
  caption_text = paste0("Achievement Level Distribution based on ", score_col)
  OUT$Caption = caption_text
  
  # Set up a data frame for ggplot2
  PLOT_df <- data.frame(
    ACHIEVEMENT_LEVEL = names(TABLE1),
    PERCENT = as.numeric(TABLE1)
  )
  
  # Make a bar plot
  PLOT1 = ggplot(PLOT_df, aes(x = ACHIEVEMENT_LEVEL, y = PERCENT)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Achievement Level Distribution", y = "Percent", x = NULL) +
    theme_minimal()
  OUT$Barplot = PLOT1
  
  # Print the barplot
  if (print_plot) print(PLOT1)
  
  # Return the new dataset
  return(invisible(OUT))
}
