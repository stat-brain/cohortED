#' Math Subset of sgpData_Long
#'
#' A subset of the `sgpData_Long` dataset, focused on student-level mathematics assessment records. 
#' This dataset can be used for educational data analysis, including mobility, achievement trends, 
#' and subgroup comparisons.
#'
#' @format A data frame with 184,724 rows and 23 variables:
#' \describe{
#'   \item{VALID_CASE}{Indicator for whether the case is valid for analysis.}
#'   \item{CONTENT_AREA}{Content area assessed; here, always "Mathematics".}
#'   \item{YEAR}{School year of the assessment (e.g., "2018_2019").}
#'   \item{ID}{Unique student identifier.}
#'   \item{LAST_NAME}{Student's last name.}
#'   \item{FIRST_NAME}{Student's first name.}
#'   \item{GRADE}{Grade level at the time of assessment (e.g., 3–8).}
#'   \item{SCALE_SCORE}{Numeric score on the mathematics assessment.}
#'   \item{ACHIEVEMENT_LEVEL}{Ordered factor representing student achievement level.}
#'   \item{GENDER}{Student gender (e.g., "Male", "Female").}
#'   \item{ETHNICITY}{Student ethnicity category.}
#'   \item{FREE_REDUCED_LUNCH_STATUS}{Free/reduced lunch eligibility status.}
#'   \item{ELL_STATUS}{English Language Learner status.}
#'   \item{IEP_STATUS}{Indicator for Individualized Education Plan.}
#'   \item{GIFTED_AND_TALENTED_PROGRAM_STATUS}{Participation in gifted/talented programs.}
#'   \item{SCHOOL_NUMBER}{Numeric school identifier.}
#'   \item{SCHOOL_NAME}{Full name of the school.}
#'   \item{EMH_LEVEL}{School level (Elementary, Middle, or High).}
#'   \item{DISTRICT_NUMBER}{Numeric district identifier.}
#'   \item{DISTRICT_NAME}{Full name of the district.}
#'   \item{SCHOOL_ENROLLMENT_STATUS}{Enrollment status at the school level.}
#'   \item{DISTRICT_ENROLLMENT_STATUS}{Enrollment status at the district level.}
#'   \item{STATE_ENROLLMENT_STATUS}{Enrollment status at the state level.}
#' }
#'
"math"