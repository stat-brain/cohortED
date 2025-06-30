#' Get Most Common Category Within Group
#'
#' Identifies the most common category within each group, based on a specified numeric value.
#' Commonly used to summarize the dominant achievement level, demographic group, etc.,
#' for each subgroup (e.g., mobility status).
#'
#' @param dataset A data frame containing a grouping variable, a category variable, and a numeric variable.
#' @param group_col A string specifying the column name for grouping (e.g., "Mobility_Status").
#' @param category_col A string specifying the column name for the category (e.g., "Achievement_Level").
#' @param value_col A string specifying the numeric column name used to rank categories (e.g., "Percent").
#'
#' @return A data frame with the most common category for each group.
#'
#' @examples
#' df <- data.frame(
#'   Mobility_Status = c("Stay", "Stay", "Join", "Join"),
#'   Achievement_Level = c("Proficient", "Partially Proficient", "Proficient", "Unsatisfactory"),
#'   Percent = c(70, 30, 40, 60)
#' )
#' get_most_common(dataset = df)
#'
#' @importFrom stats aggregate
#' @export
#'

get_most_common <- function(dataset,
                            group_col = "Mobility_Status",
                            category_col = "Achievement_Level",
                            value_col = "Percent") {
  # Aggregate values by group and category
  agg <- stats::aggregate(dataset[[value_col]],
                          by = list(dataset[[group_col]], dataset[[category_col]]),
                          FUN = sum)
  names(agg) <- c("Group", "Category", "Value")
  
  # For each group, find the category with the highest value
  result <- do.call(rbind, lapply(split(agg, agg$Group), function(sub) {
    sub[which.max(sub$Value), c("Group", "Category")]
  }))
  
  # Rename columns to match original input names
  names(result) <- c(group_col, category_col)
  rownames(result) <- NULL
  
  return(result)
}
