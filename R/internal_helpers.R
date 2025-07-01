#' Normalize grade levels to numeric values for analysis (internal use)
#'
#' Converts grade representations such as "PK", "PreK", and "K" to numeric codes:
#' - "PK" or "PreK" become -1
#' - "K" becomes 0
#' - Numeric grades remain as numeric values
#'
#' @param grade A vector of grade values (character or numeric)
#' @return A numeric vector with normalized grade values
#' @keywords internal
#'

.normalize_grade <- function(grade) {
  grade_char <- toupper(as.character(grade))
  
  normalized <- ifelse(
    grade_char %in% c("PK", "PREK"), -1,
    ifelse(
      grade_char == "K", 0,
      suppressWarnings(as.numeric(grade_char))
    )
  )
  
  return(normalized)
}


#' Internal helper to parse various year input formats
#'
#' Attempts to extract the start numeric year from inputs like:
#' - numeric year (e.g. 2021)
#' - string year (e.g. "2021", "2021_2022", "2021-2022", "2021/22")
#'
#' @param year_input Numeric or string representing a year or academic year range
#' @return Integer start year
#' @keywords internal
#'

.parse_year <- function(year_input) {
  sapply(year_input, function(yi) {
    if (is.numeric(yi)) {
      return(as.integer(yi))
    }
    
    if (is.character(yi)) {
      start_year <- suppressWarnings(as.integer(substr(yi, 1, 4)))
      if (!is.na(start_year)) {
        return(start_year)
      }
      
      pattern <- "^\\s*(\\d{4})\\s*[-_/]\\s*(\\d{2,4})\\s*$"
      m <- regexec(pattern, yi)
      matches <- regmatches(yi, m)
      
      if (length(matches[[1]]) >= 2) {
        return(as.integer(matches[[1]][2]))
      }
    }
    
    return(NA_integer_)  # fallback if parsing fails
  })
}


#' Get Most Common Category Within Group (internal use)
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
#' @keywords internal
#' 

.get_most_common <- function(dataset,
                             group_col = "Mobility_Status",
                             category_col = "Achievement_Level",
                             value_col = "Percent") {
  agg <- stats::aggregate(dataset[[value_col]],
                          by = list(dataset[[group_col]], dataset[[category_col]]),
                          FUN = sum)
  names(agg) <- c("Group", "Category", "Value")
  
  result <- do.call(rbind, lapply(split(agg, agg$Group), function(sub) {
    sub[which.max(sub$Value), c("Group", "Category")]
  }))
  
  names(result) <- c(group_col, category_col)
  rownames(result) <- NULL
  
  return(result)
}

#' Convert Numeric Year to Academic Year String
#'
#' Converts a numeric year (e.g., 2019) to a formatted academic year string
#' (e.g., "2019-2020"). Handles vector inputs as well.
#'
#' This function is intended for internal use within the package.
#'
#' @param year_numeric A numeric vector of years representing the start year.
#'
#' @return A character vector of academic year strings in the form "YYYY-YYYY+1".
#' 
#' @keywords internal
#'

.to_academic_year <- function(year_numeric) {
  if (!is.numeric(year_numeric)) {
    stop("Input to .to_academic_year() must be numeric.")
  }
  paste0(year_numeric, "-", year_numeric + 1)
}
