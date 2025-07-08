#' @title Glossary of Common Definitions
#' 
#' @description 
#' A named list containing commonly used definitions in reporting. Each category (e.g., `mobility`) 
#' includes:
#' 
#' - `items`: a named list of plain-language definitions  
#' - `bullet_text`: a markdown-formatted paragraph for easy rendering in Quarto reports
#' 
#' This structure supports consistency and readability across multiple report types.
#'
#' @format A named list with top-level elements such as `mobility`
#' @keywords internal
#' @export
#' 

definitions <- list()

definitions$mobility <- list(
  items = list(
    Leave = "Enrolled last year but not enrolled this year.",
    Stay = "Enrolled in both last year and this year.",
    Join = "Enrolled this year but not last year.",
    Repeat = "In the same grade two years in a row.",
    Advance = "Skipped one or more grades between years."
  )
)

definitions$mobility$bullet_text <- paste(
  "A student has a mobility status of:",
  "",
  paste0("- **", names(definitions$mobility$items), "**: ", definitions$mobility$items),
  collapse = "\n"
)

definitions$achievement_levels = list(
    items = list(
      Unsatisfactory = "The student did not meet grade-level expectations.", 
      Partially_Proficient = "The student demonstrated partial understanding.", 
      Proficient = "The student met grade-level expectations.", 
      Advanced = "The student exceeded grade-level expectations."
    )
)

definitions$achievement_levels$bullet_text = paste(
  "Achievement levels are categorized as follows:", 
  "", 
  paste0("- **", names(definitions$achievement_levels$items), "**: ", definitions$achievement_levels$items),
  collapse = "\n"
)
