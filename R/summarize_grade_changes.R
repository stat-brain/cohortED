#' @title Summarize Student Grade Transitions and Enrollment Patterns
#'
#' @description
#' Identifies and summarizes student grade-level transitions across academic years.
#' This function classifies transitions such as grade repetition, Skipping, and dropping back,
#' and also detects non-contiguous enrollment patterns (returns).
#'
#' @param dataset A data frame containing at least the columns `ID`, `GRADE`, and `YEAR`.
#'
#' @return A named list invisibly containing:
#'
#' - **Student_Transitions**: A cleaned student-level dataset with derived columns:
#'   - `GRADE_CHANGE`: Numeric change in grade from prior year
#'   - `YEAR_CHANGE`: Numeric change in year
#'   - `GRADE_GAP`: Grade change minus year change
#'   - `GRADE_TRANSITION`: Categorized as `"Stay"`, `"Repeat"`, `"Skip"`, or `"Drop_Back"`
#'   - `ENROLLMENT_TRANSITION`: `"Enrolled"` or `"Return"` based on year continuity
#'   - `IS_FINAL`: Marks final row for each student
#'   - `ACADEMIC_YEAR`: Computed label from `YEAR`
#'
#' - **Summary_Tables**: A list of summary tables:
#'   - `By_Grade`: Count of each grade transition type
#'   - `By_Year_And_Grade`: Grade transitions by academic year (excluding first year)
#'   - `By_Year_And_Enrollment`: Enrollment status by academic year
#'   - `Note`: Explains why no transitions are shown for the first observed year
#'
#' - **Details**:
#'   - `Conflicts`: Student-year records with multiple grade levels (if any)
#'   - `Enrollment_Transitions`: Total counts of `"Enrolled"` and `"Return"` statuses
#'   - `Summary_Tables$By_Year_And_Enrollment`: Detailed year-level enrollment summary
#'   - `Notes`: Informational messages about the return group (if applicable)
#'
#' - **Data**:
#'   - `Return`: Subset of records for students who returned after missing years
#'   - `Stay`, `Repeat`, `Skip`, `Drop_Back`: Subsets of records based on grade transition type
#'
#' @details
#' Grade transitions are classified relative to expected year-over-year progress.
#' A student who Skips a grade (e.g., from 3 to 5) is labeled as `"Skip"`, while a student
#' who Repeats a grade (e.g., 3 to 3) is labeled as `"Repeat"`. A `"Return"` enrollment
#' transition indicates a break in year continuity (e.g., a student is observed in 2019 and 2021 but not 2020).
#'
#' Grade transitions are not reported for the first observed year in the dataset, as there is no
#' prior year for comparison.
#'
#' @examples
#' summarize_grade_transitions(dataset = math)
#'
#' @export
#' 

summarize_grade_changes <- function(dataset) {
  #--- Input checks ---
  if (!is.data.frame(dataset)) stop("'dataset' must be a data frame.")
  
  required_vars <- c("ID", "GRADE", "YEAR")
  missing <- setdiff(required_vars, names(dataset))
  if (length(missing) > 0) stop(paste("Missing required variables:", paste(missing, collapse = ", ")))
  
  #--- Prepare output ---
  OUT <- list()
  OUT$Details <- list()
  
  #--- Define working data frame ---
  df <- dataset
  
  #--- Detect conflicts: multiple grades per student-year ---
  grade_counts <- aggregate(GRADE ~ ID + YEAR, data = df, FUN = function(g) length(unique(g)))
  conflicts <- subset(grade_counts, GRADE > 1)
  names(conflicts)[names(conflicts) == "GRADE"] <- "Num_Unique_Grades"
  
  if (nrow(conflicts) > 0) {
    conflicts_df <- merge(df, conflicts, by = c("ID", "YEAR"))
    conflicts_df <- conflicts_df[order(conflicts_df$ID, conflicts_df$YEAR), ]
    message(sprintf(
      "Note: %s student-year records had multiple grades. The most common grade was used. See `Details$Conflicts` in the output for review.",
      nrow(conflicts_df)
    ))
    OUT$Details$Conflicts <- conflicts_df
  } else {
    OUT$Details$Conflicts <- "There were no grade level conflicts."
  }
  
  #--- Collapse to most common grade per student-year ---
  df <- aggregate(GRADE ~ ID + YEAR, data = df, FUN = get_mode)
  
  #--- Normalize and prepare ---
  df$YEAR <- parse_year(df$YEAR)
  df$GRADE_NUM <- .normalize_grade(df$GRADE)
  df <- df[order(df$ID, df$YEAR), ]
  df_split <- split(df, df$ID)
  
  #--- Compute transitions ---
  result_list <- lapply(df_split, function(subdf) {
    n <- nrow(subdf)
    subdf$GRADE_CHANGE <- c(NA, diff(subdf$GRADE_NUM))
    subdf$YEAR_CHANGE <- c(NA, diff(subdf$YEAR))
    subdf$GRADE_GAP <- subdf$GRADE_CHANGE - subdf$YEAR_CHANGE
    subdf$GRADE_TRANSITION <- NA
    subdf$ENROLLMENT_TRANSITION <- NA
    subdf$IS_FINAL <- FALSE
    
    for (i in seq_len(n)) {
      if (i == 1) {
        subdf$ENROLLMENT_TRANSITION[i] <- "Enrolled"
        next
      }
      if (subdf$YEAR_CHANGE[i] > 1) {
        subdf$ENROLLMENT_TRANSITION[i] <- "Return"
      }
      gap <- subdf$GRADE_GAP[i]
      subdf$GRADE_TRANSITION[i] <- if (gap == 0) {
        "Stay"
      } else if (gap == -1) {
        "Repeat"
      } else if (gap > 0) {
        "Skip"
      } else if (gap < -1) {
        "Drop_Back"
      } else {
        NA
      }
    }
    
    subdf$IS_FINAL[n] <- TRUE
    return(subdf)
  })
  
  transitions <- do.call(rbind, result_list)
  rownames(transitions) <- NULL
  
  #--- Add academic year label ---
  transitions$ACADEMIC_YEAR <- to_academic_year(transitions$YEAR)
  
  #--- Set transition columns as factors ---
  transitions$GRADE_TRANSITION <- factor(transitions$GRADE_TRANSITION,
                                         levels = c("Stay", "Repeat", "Skip", "Drop_Back"))
  transitions$ENROLLMENT_TRANSITION <- factor(transitions$ENROLLMENT_TRANSITION,
                                              levels = c("Enrolled", "Return"))
  
  #--- Save to output ---
  OUT$Student_Transitions <- transitions
  
  #--- Summary tables ---
  OUT$Summary_Tables <- list()
  
  OUT$Summary_Tables$By_Grade <- as.data.frame(table(transitions$GRADE_TRANSITION), stringsAsFactors = FALSE)
  
  enrollment_summary <- as.data.frame(table(transitions$ENROLLMENT_TRANSITION), stringsAsFactors = FALSE)
  names(enrollment_summary) <- c("Enrollment_Status", "Freq")
  
  OUT$Details$Enrollment_Transitions <- enrollment_summary
  
  summary_year_grade <- as.data.frame(table(
    Academic_Year = transitions$ACADEMIC_YEAR,
    GRADE_TRANSITION = transitions$GRADE_TRANSITION
  ), stringsAsFactors = FALSE)
  summary_year_grade$Total <- ave(summary_year_grade$Freq, summary_year_grade$Academic_Year, FUN = sum)
  summary_year_grade$Percent <- round(100 * summary_year_grade$Freq / summary_year_grade$Total, 1)
  summary_year_grade <- subset(summary_year_grade, Total > 0)
  summary_year_grade <- summary_year_grade[order(summary_year_grade$Academic_Year, summary_year_grade$GRADE_TRANSITION), ]
  rownames(summary_year_grade) <- NULL
  OUT$Summary_Tables$By_Year_And_Grade <- summary_year_grade
  
  enroll_by_year <- as.data.frame(table(
    Year = transitions$YEAR,
    Enrollment_Transition = transitions$ENROLLMENT_TRANSITION
  ), stringsAsFactors = FALSE)
  enroll_by_year$Total <- ave(enroll_by_year$Freq, enroll_by_year$Year, FUN = sum)
  enroll_by_year$Percent <- round(100 * enroll_by_year$Freq / enroll_by_year$Total, 1)
  enroll_by_year$Academic_Year <- to_academic_year(as.numeric(enroll_by_year$Year))
  enroll_by_year <- enroll_by_year[order(enroll_by_year$Year, enroll_by_year$Enrollment_Transition), ]
  rownames(enroll_by_year) <- NULL
  enroll_by_year <- enroll_by_year[, c("Academic_Year", "Enrollment_Transition", "Freq", "Total", "Percent")]
  OUT$Summary_Tables$By_Year_And_Enrollment <- enroll_by_year
  
  first_year <- min(transitions$YEAR, na.rm = TRUE)
  OUT$Summary_Tables$Note <- paste0(
    "Grade transitions are not reported for the first year in the dataset (", 
    to_academic_year(first_year), 
    "), as no prior data exists for comparison."
  )
  
  #--- Return student-level records ---
  OUT$Data <- list()
  df$ID <- as.character(df$ID)
  return_ids <- unique(as.character(transitions$ID[transitions$ENROLLMENT_TRANSITION == "Return"]))
  
  if (length(return_ids) > 0) {
    OUT$Data$Return <- dataset[dataset$ID %in% return_ids, ]
    rownames(OUT$Data$Return) <- NULL
    OUT$Details$Notes <- c(
      OUT$Details$Notes,
      sprintf("Return group includes %d students with non-contiguous enrollment years.", length(return_ids))
    )
  } else {
    OUT$Data$Return <- NULL
  }
  
  #--- Grade transition-based student records ---
  grade_labels <- c("Stay", "Repeat", "Skip", "Drop_Back")
  for (label in grade_labels) {
    ids <- unique(transitions$ID[transitions$GRADE_TRANSITION == label])
    label_cap <- paste0(toupper(substring(label, 1, 1)), substring(label, 2))
    if (length(ids) > 0) {
      OUT$Data[[label_cap]] <- dataset[dataset$ID %in% ids, ]
      rownames(OUT$Data[[label_cap]]) <- NULL
      OUT$Data[[label_cap]]$GRADE_NUM <- NULL 
    } else {
      OUT$Data[[label_cap]] <- NULL
    }
  }
  
  return(invisible(OUT))
}

