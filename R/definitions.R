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

definitions$mobility$bullets <- paste0(
  "- **", names(definitions$mobility$items), "**: ", definitions$mobility$items
)

# Step 2: Join with intro using \n separator
definitions$mobility$bullet_text <- paste(
  "A student has a mobility status of:",
  "",
  paste(definitions$mobility$bullets, collapse = "\n"),
  sep = "\n"
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

definitions$trajectory <- list(
  items = list(
    Trajectory = paste(
      "A student's observed sequence of grade-level placements across academic years,",
      "starting from their first appearance in the dataset.",
      "In `cohortED`, trajectories assume linear grade progression (one grade per year).",
      "Students who skip or repeat a grade are treated as exiting the cohort at the point of deviation,",
      "unless handled by specialized functions."
    )
  )
)

definitions$trajectory$bullets <- paste0(
  "- **", names(definitions$trajectory$items), "**: ", definitions$trajectory$items
)

definitions$trajectory$bullet_text <- paste(
  "In cohortED, a trajectory is defined as:",
  "",
  paste(definitions$trajectory$bullets, collapse = "\n"),
  sep = "\n"
)

definitions$transitions <- list(
  items = list(
    Enrolled = "Student appears in consecutive academic years or for the first time.",
    Return = "Student reappears in the dataset after missing one or more years.",
    Stay = 'Advanced one grade level over one year (typical progression). Synonymous with the definition of "Stay" for Cohorts.',
    Repeat = "Remained in the same grade across two consecutive years.",
    Skip = "Advanced more than one grade level in a single year.",
    Drop_Back = "Moved backward in grade level or regressed after a gap."
  )
)

definitions$transitions$bullets <- paste0(
  "- **", names(definitions$transitions$items), "**: ", definitions$transitions$items
)

definitions$transitions$bullet_text <- paste(
  "Enrollment and grade transitions are classified as follows:",
  "",
  paste(definitions$transitions$bullets, collapse = "\n"),
  sep = "\n"
)
