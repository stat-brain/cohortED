







compare_achievement_mobility <- function(dataset, current_year, current_grade) {
  # Internal call to obtain transformed data
  NEW_DATA = plot_alluvial_mobility(dataset = dataset, 
                                    current_year = current_year, 
                                    current_grade = current_grade, 
                                    print_plot = FALSE)$Data
  
  # Separating the 
  DATA.x = NEW_DATA[!is.na(NEW_DATA$ACHIEVEMENT_LEVEL.x) & 
                      NEW_DATA$ACHIEVEMENT_LEVEL.x != "No Score", ]
  
  
  
  
}