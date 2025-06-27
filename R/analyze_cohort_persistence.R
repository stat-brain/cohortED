






analyze_cohort_persistence = function(dataset, start_grade, start_year, end_grade = NULL) {
  # Validate and standardize column names
  colnames_lower <- tolower(names(dataset))
  
  # Required variables
  required_variables <- c("id", "grade", "year")
  missing_variables <- required_variables[!required_variables %in% colnames_lower]
  if(length(missing_variables) > 0) {
    stop(paste("Missing required variable(s):", paste(missing_variables, collapse = ", ")))
  }
  
  # Map the column names
  name_map <- setNames(names(dataset), colnames_lower)
  id_col <- name_map["id"]
  grade_col <- name_map["grade"]
  year_col <- name_map["year"]
  
  # Prepare the output
  OUT <- list()
  
  # Handle the optional argument
  if(is.null(end_grade)) {
    end_grade = 12
  }
  
  # Normalize grade values to numeric for sorting and manipulation
  dataset[[grade_col]] <- normalize_grade(dataset[[grade_col]])
  
  # Ensure YEAR is character and extract numeric start year
  dataset[[year_col]] <- as.character(dataset[[year_col]])
  dataset[[year_col]] <- as.numeric(substr(dataset[[year_col]], 1, 4))
  
  # Ensure that the start_grade is numeric quantity
  start_grade <- as.numeric(normalize_grade(start_grade))
  
  # Get the grade sequence
  grade_sequence <- seq(from = start_grade, to = end_grade, by = 1)
  
  # Determine the number of years
  N_years <- length(grade_sequence)
  
  # Ensure YEAR is character and extract numeric start year
  start_year <- as.character(start_year)
  start_year <- as.numeric(substr(start_year, 1, 4))
  
  # Get the year sequence
  year_sequence <- seq(from = start_year, length.out = N_years, by = 1)
  
  # Repository to hold portions of the dataset while in use
  TEMP_DATA = list()
  N_PERSIST = numeric()
  
  # for loop to isolate the datasets
  for(i in 1:N_years) {
    TEMP_DATA[[i]] = dataset[dataset[[year_col]] == year_sequence[i] & dataset[[grade_col]] == grade_sequence[i], ]
  }
  
  N_PERSIST[1] = nrow(TEMP_DATA[[1]])
  
  # Dataset for output
  PERSIST = list()
  
  # Start the persistence dataset
  PERSIST_ID <- intersect(x = TEMP_DATA[[1]][[id_col]], y = TEMP_DATA[[2]][[id_col]])
  PERSIST[[1]] = TEMP_DATA[[2]][TEMP_DATA[[2]][[id_col]] %in% PERSIST_ID, ]
  N_PERSIST[2] = length(PERSIST_ID)
  
  # for loop to track the cohort over time
  for(j in 2:(N_years - 1)) {
    PERSIST_ID <- intersect(x = PERSIST_ID, y = TEMP_DATA[[(j+1)]][[id_col]])
    PERSIST[[j]] = TEMP_DATA[[(j+1)]][TEMP_DATA[[(j+1)]][[id_col]] %in% PERSIST_ID, ]
    N_PERSIST[(j+1)] = length(PERSIST_ID)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}